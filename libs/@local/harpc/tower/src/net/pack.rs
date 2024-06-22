use core::{
    pin::Pin,
    task::{ready, Context, Poll},
};

use bytes::{Buf, BufMut, Bytes, BytesMut};
use futures::Stream;
use harpc_net::session::error::TransactionError;
use harpc_wire_protocol::response::kind::{ErrorCode, ResponseKind};

use crate::body::{Body, Frame};

struct TransactionErrorMut {
    code: ErrorCode,
    bytes: BytesMut,
}

impl From<TransactionErrorMut> for TransactionError {
    fn from(error: TransactionErrorMut) -> Self {
        TransactionError {
            code: error.code,
            bytes: error.bytes.freeze(),
        }
    }
}

pin_project_lite::pin_project! {
    pub struct Pack<B> {
        #[pin]
        inner: B,
        error: Option<TransactionErrorMut>,
        exhausted: bool,
    }
}

impl<B> Stream for Pack<B>
where
    B: Body<Control: AsRef<ResponseKind>, Error = !>,
{
    type Item = Result<Bytes, TransactionError>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        if self.exhausted {
            return Poll::Ready(None);
        }

        let this = self.project();

        let next = ready!(this.inner.poll_frame(cx));

        match next {
            None => {
                let error = this.error.take();
                *this.exhausted = true;

                Poll::Ready(error.map(TransactionError::from).map(Err))
            }
            Some(Ok(Frame::Data(data))) => {
                if let Some(error) = this.error.as_mut() {
                    // errors need to be sent to the stream as a single frame, so we accumulate
                    error.bytes.put(data);

                    Poll::Pending
                } else {
                    let mut bytes = BytesMut::with_capacity(data.remaining());
                    bytes.put(data);
                    let bytes = bytes.freeze();

                    Poll::Ready(Some(Ok(bytes)))
                }
            }
            Some(Ok(Frame::Control(control))) => {
                let kind = *control.as_ref();

                match kind {
                    ResponseKind::Err(code) => {
                        // if we have a previous error, finish said error and return it, otherwise
                        // wait for the next frame to populate it
                        let active = this.error.replace(TransactionErrorMut {
                            code,
                            bytes: BytesMut::new(),
                        });

                        if let Some(active) = active {
                            Poll::Ready(Some(Err(active.into())))
                        } else {
                            Poll::Pending
                        }
                    }
                    ResponseKind::Ok => {
                        // take the old error and return it (if it exists), otherwise pending
                        // if we wouldn't do that we would concatenate valid values to the error
                        let error = this.error.take();

                        if let Some(error) = error {
                            Poll::Ready(Some(Err(error.into())))
                        } else {
                            Poll::Pending
                        }
                    }
                }
            }
        }
    }
}
