use crate::{Lit, Offset};
use core::fmt::{self, Display};

/// A litmat is either a literal or a matrix.
///
/// This is used for nonclausal proof search.
#[derive(Clone, Debug)]
pub enum LitMat<L, M> {
    /// literal
    Lit(L),
    /// matrix
    Mat(M),
}

impl<L, M> LitMat<L, M> {
    /// Return literal if the litmat is one.
    pub fn lit(self) -> Option<L> {
        match self {
            Self::Lit(l) => Some(l),
            Self::Mat(_) => None,
        }
    }
}

impl<P, C, V, M> LitMat<Lit<P, C, V>, M> {
    /// A litmat is ground if it is a ground literal.
    pub fn is_ground(&self) -> bool {
        match self {
            Self::Lit(l) => l.is_ground(),
            Self::Mat(_) => false,
        }
    }
}

impl<L: Display, M: Display> Display for LitMat<L, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lit(l) => l.fmt(f),
            Self::Mat(m) => m.fmt(f),
        }
    }
}

impl<'t, L: 't, M: 't> Display for Offset<&'t LitMat<L, M>>
where
    Offset<&'t L>: Display,
    Offset<&'t M>: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.transpose().fmt(f)
    }
}
