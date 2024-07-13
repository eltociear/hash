use core::task::ready;
use std::{
    pin::Pin,
    task::{Context, Poll},
};

use bytes::Buf;
use futures::Stream;

use super::{Body, Frame};

pin_project_lite::pin_project! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct StreamBody<S> {
        #[pin]
        stream: S,
        is_complete: bool,
    }
}

impl<S> StreamBody<S> {
    pub fn new(stream: S) -> Self {
        Self {
            stream,
            is_complete: false,
        }
    }
}

impl<S, D, C, E> Body for StreamBody<S>
where
    S: Stream<Item = Result<Frame<D, C>, E>>,
    D: Buf,
{
    type Control = C;
    type Data = D;
    type Error = E;

    fn poll_frame(
        self: Pin<&mut Self>,
        cx: &mut Context,
    ) -> Poll<Option<super::BodyFrameResult<Self>>> {
        let this = self.project();

        let value = ready!(this.stream.poll_next(cx));

        if value.is_none() {
            *this.is_complete = true;
        }

        Poll::Ready(value)
    }

    fn is_complete(&self) -> Option<bool> {
        if self.is_complete { Some(true) } else { None }
    }
}

pin_project_lite::pin_project! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BodyStream<B> {
        #[pin]
        inner: B,
    }
}

impl<B> BodyStream<B> {
    pub fn new(inner: B) -> Self {
        Self { inner }
    }

    pub fn into_data_stream(self) -> BodyDataStream<B> {
        BodyDataStream { inner: self.inner }
    }

    pub fn into_control_stream(self) -> BodyControlStream<B> {
        BodyControlStream { inner: self.inner }
    }
}

impl<B> Stream for BodyStream<B>
where
    B: Body,
{
    type Item = Result<Frame<B::Data, B::Control>, B::Error>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        return self.project().inner.poll_frame(cx);
    }
}

pin_project_lite::pin_project! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BodyDataStream<B> {
        #[pin]
        inner: B,
    }
}

impl<B> Stream for BodyDataStream<B>
where
    B: Body,
{
    type Item = Result<B::Data, B::Error>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        loop {
            return match ready!(self.as_mut().project().inner.poll_frame(cx)) {
                Some(Ok(frame)) => match frame.into_data() {
                    Ok(data) => Poll::Ready(Some(Ok(data))),
                    Err(_) => continue,
                },
                Some(Err(err)) => Poll::Ready(Some(Err(err))),
                None => Poll::Ready(None),
            };
        }
    }
}

pin_project_lite::pin_project! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct BodyControlStream<B> {
        #[pin]
        inner: B,
    }
}

impl<B> Stream for BodyControlStream<B>
where
    B: Body,
{
    type Item = Result<B::Control, B::Error>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        loop {
            return match ready!(self.as_mut().project().inner.poll_frame(cx)) {
                Some(Ok(frame)) => match frame.into_control() {
                    Ok(control) => Poll::Ready(Some(Ok(control))),
                    Err(_) => continue,
                },
                Some(Err(err)) => Poll::Ready(Some(Err(err))),
                None => Poll::Ready(None),
            };
        }
    }
}

#[cfg(test)]
mod test {
    use core::task::Poll;

    use bytes::Bytes;
    use futures::stream;

    use super::{BodyStream, StreamBody};
    use crate::body::{
        test::{poll_frame_unpin, poll_stream_unpin},
        Body, Frame,
    };

    const A: &[u8] = b"hello";
    const B: &[u8] = b"world";
    const C: &[u8] = b"!";

    #[test]
    fn body_from_stream() {
        let stream = stream::iter([
            Ok(Frame::Data(Bytes::from_static(A))),
            Ok(Frame::Data(Bytes::from_static(B))),
            Ok(Frame::Data(Bytes::from_static(C))),
        ] as [Result<Frame<_, !>, !>; 3]);
        let mut body = StreamBody::new(stream);

        let frame = poll_frame_unpin(&mut body);
        assert_eq!(
            frame,
            Poll::Ready(Some(Ok(Frame::Data(Bytes::from_static(A)))))
        );
        assert_eq!(body.is_complete(), None);

        let frame = poll_frame_unpin(&mut body);
        assert_eq!(
            frame,
            Poll::Ready(Some(Ok(Frame::Data(Bytes::from_static(B)))))
        );
        assert_eq!(body.is_complete(), None);

        let frame = poll_frame_unpin(&mut body);
        assert_eq!(
            frame,
            Poll::Ready(Some(Ok(Frame::Data(Bytes::from_static(C)))))
        );
        assert_eq!(body.is_complete(), None);

        let frame = poll_frame_unpin(&mut body);
        assert_eq!(frame, Poll::Ready(None));
        assert_eq!(body.is_complete(), Some(true));
    }

    #[test]
    fn stream_from_body() {
        let stream = stream::iter([
            Ok(Frame::Data(Bytes::from_static(A))),
            Ok(Frame::Data(Bytes::from_static(B))),
            Ok(Frame::Data(Bytes::from_static(C))),
        ] as [Result<Frame<_, !>, !>; 3]);
        let body = StreamBody::new(stream);
        let mut stream = BodyStream::new(body);

        assert_eq!(
            poll_stream_unpin(&mut stream),
            Poll::Ready(Some(Ok(Frame::Data(Bytes::from_static(A)))))
        );

        assert_eq!(
            poll_stream_unpin(&mut stream),
            Poll::Ready(Some(Ok(Frame::Data(Bytes::from_static(B)))))
        );

        assert_eq!(
            poll_stream_unpin(&mut stream),
            Poll::Ready(Some(Ok(Frame::Data(Bytes::from_static(C)))))
        );

        assert_eq!(poll_stream_unpin(&mut stream), Poll::Ready(None));
    }
}
