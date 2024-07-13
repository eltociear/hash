use core::task::ready;
use std::{
    pin::Pin,
    task::{Context, Poll},
};

use super::{Body, Frame, SizeHint};

pin_project_lite::pin_project! {
    pub struct Controlled<C, B> {
        control: Option<C>,
        #[pin]
        body: B,
    }
}

impl<C, B> Controlled<C, B> {
    pub fn new(control: C, body: B) -> Self {
        Self {
            control: Some(control),
            body,
        }
    }
}

impl<C, B> Body for Controlled<C, B>
where
    B: Body<Control = !>,
    C: From<!>,
{
    type Control = C;
    type Data = B::Data;
    type Error = B::Error;

    fn poll_frame(
        self: Pin<&mut Self>,
        cx: &mut Context,
    ) -> Poll<Option<Result<Frame<Self::Data, Self::Control>, Self::Error>>> {
        let this = self.project();

        if let Some(control) = this.control.take() {
            return Poll::Ready(Some(Ok(Frame::new_control(control))));
        }

        let Some(result) = ready!(this.body.poll_frame(cx)) else {
            return Poll::Ready(None);
        };

        Poll::Ready(Some(result.map(|frame| frame.map_control(From::from))))
    }

    fn is_complete(&self) -> Option<bool> {
        if self.control.is_some() {
            return None;
        }

        self.body.is_complete()
    }

    fn size_hint(&self) -> SizeHint {
        self.body.size_hint()
    }
}

#[cfg(test)]
mod test {
    use core::task::Poll;

    use bytes::Bytes;

    use super::Controlled;
    use crate::body::{full::Full, test::poll_frame_unpin, Body, Frame, SizeHint};

    #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct Control;

    impl From<!> for Control {
        fn from(value: !) -> Self {
            match value {}
        }
    }

    #[test]
    fn poll_frame() {
        let bytes = Bytes::from("hello");

        let mut body = Controlled::new(Control, Full::new(bytes.clone()));

        let frame = poll_frame_unpin(&mut body);
        assert_eq!(frame, Poll::Ready(Some(Ok(Frame::Control(Control)))));

        let frame = poll_frame_unpin(&mut body);
        assert_eq!(frame, Poll::Ready(Some(Ok(Frame::Data(bytes)))));

        let frame = poll_frame_unpin(&mut body);
        assert_eq!(frame, Poll::Ready(None));
    }

    #[test]
    fn is_complete() {
        let mut body = Controlled::new(Control, Full::new(Bytes::from("hello")));
        assert_eq!(body.is_complete(), None);

        let _ = poll_frame_unpin(&mut body);
        assert_eq!(body.is_complete(), None);

        let _ = poll_frame_unpin(&mut body);
        assert_eq!(body.is_complete(), Some(true));
    }

    #[test]
    fn size_hint() {
        let bytes = Bytes::from("hello");

        let mut body = Controlled::new(Control, Full::new(bytes.clone()));
        assert_eq!(body.size_hint(), SizeHint::with_exact(5));

        let _ = poll_frame_unpin(&mut body);
        assert_eq!(body.size_hint(), SizeHint::with_exact(5));

        let _ = poll_frame_unpin(&mut body);
        assert_eq!(body.size_hint(), SizeHint::with_exact(0));
    }
}
