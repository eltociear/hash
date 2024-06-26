use core::{
    fmt::{self, Debug, Formatter},
    pin::Pin,
    task::{Context, Poll},
};

use bytes::Buf;

use super::{empty::Empty, Body, Frame};

pub struct BoxBody<D, C, E> {
    inner: Pin<Box<dyn Body<Data = D, Control = C, Error = E> + Send + Sync + 'static>>,
}

impl<D, C, E> BoxBody<D, C, E> {
    pub fn new<B>(body: B) -> Self
    where
        B: Body<Data = D, Control = C, Error = E> + Send + Sync + 'static,
        D: Buf,
    {
        Self {
            inner: Box::pin(body),
        }
    }
}

impl<D, C, E> Debug for BoxBody<D, C, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("BoxBody").finish()
    }
}

impl<D, C, E> Body for BoxBody<D, C, E>
where
    D: Buf,
{
    type Control = C;
    type Data = D;
    type Error = E;

    fn poll_frame(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
    ) -> Poll<Option<Result<Frame<Self::Data, Self::Control>, Self::Error>>> {
        self.inner.as_mut().poll_frame(cx)
    }

    fn is_complete(&self) -> Option<bool> {
        self.inner.is_complete()
    }

    fn size_hint(&self) -> super::SizeHint {
        self.inner.size_hint()
    }
}

impl<D> Default for BoxBody<D, !, !>
where
    D: Buf + 'static,
{
    fn default() -> Self {
        Self::new(Empty::new())
    }
}

pub struct UnsyncBoxBody<D, C, E> {
    inner: Pin<Box<dyn Body<Data = D, Control = C, Error = E> + Send + 'static>>,
}

impl<D, C, E> UnsyncBoxBody<D, C, E> {
    pub fn new<B>(body: B) -> Self
    where
        B: Body<Data = D, Control = C, Error = E> + Send + 'static,
        D: Buf,
    {
        Self {
            inner: Box::pin(body),
        }
    }
}

impl<D, C, E> Debug for UnsyncBoxBody<D, C, E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("UnsyncBoxBody").finish()
    }
}

impl<D, C, E> Body for UnsyncBoxBody<D, C, E>
where
    D: Buf,
{
    type Control = C;
    type Data = D;
    type Error = E;

    fn poll_frame(
        mut self: Pin<&mut Self>,
        cx: &mut Context,
    ) -> Poll<Option<Result<Frame<Self::Data, Self::Control>, Self::Error>>> {
        self.inner.as_mut().poll_frame(cx)
    }

    fn is_complete(&self) -> Option<bool> {
        self.inner.is_complete()
    }

    fn size_hint(&self) -> super::SizeHint {
        self.inner.size_hint()
    }
}

impl<D> Default for UnsyncBoxBody<D, !, !>
where
    D: Buf + 'static,
{
    fn default() -> Self {
        Self::new(Empty::new())
    }
}
