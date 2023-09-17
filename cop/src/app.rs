use core::fmt::{self, Display};
use core::ops::Neg;

/// Application of argument(s) to a head.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct App<H, A>(H, A);

impl<H: Neg<Output = H>, A> Neg for App<H, A> {
    type Output = Self;
    fn neg(self) -> Self {
        Self(-self.0, self.1)
    }
}

impl<H: Display, A: Display> Display for App<H, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

impl<H, A> App<H, A> {
    /// Create a new application of arguments to a head symbol.
    pub fn new(head: H, args: A) -> Self {
        Self(head, args)
    }

    /// Get the head symbol.
    pub fn head(&self) -> &H {
        &self.0
    }

    /// Get the arguments.
    pub fn args(&self) -> &A {
        &self.1
    }

    /// Apply a function to the head.
    pub fn map_head<I>(self, f: impl FnOnce(H) -> I) -> App<I, A> {
        App(f(self.0), self.1)
    }

    /// Apply a function to the arguments.
    pub fn map_args<B>(self, f: impl FnOnce(A) -> B) -> App<H, B> {
        App(self.0, f(self.1))
    }
}
