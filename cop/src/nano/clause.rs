use super::Matrix;
use crate::LitMat;
use alloc::{boxed::Box, vec::Vec};
use core::fmt::{self, Display};

pub type Clause<L, M> = crate::Clause<LitMat<L, M>>;

#[derive(Debug)]
pub struct VClause<L, V>(pub Vec<V>, pub Clause<L, Matrix<L, V>>);

impl<L, M> Clause<L, M> {
    /// Return the matrices contained in a clause.
    fn mats(&self) -> impl Iterator<Item = &M> {
        self.iter().filter_map(|lm| match lm {
            LitMat::Lit(_) => None,
            LitMat::Mat(m) => Some(m),
        })
    }
}

impl<L, V> Matrix<L, V> {
    pub fn bound_vars(&self) -> impl Iterator<Item = &V> {
        self.into_iter().flat_map(|c| c.bound_vars())
    }
}

impl<L, V> Clause<L, Matrix<L, V>> {
    /// Return all variables bound somewhere in the clause.
    ///
    /// This does not look at the variables that might occur free in the literals!
    /// Therefore, it makes sense to call this function only on outermost clauses.
    pub fn bound_vars(&self) -> impl Iterator<Item = &V> {
        self.mats().flat_map(|m| m.bound_vars())
    }
}

impl<L, V> VClause<L, V> {
    pub fn bound_vars(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        Box::new(self.0.iter().chain(self.1.bound_vars()))
    }
}

impl<L, V> From<Matrix<L, V>> for Clause<L, Matrix<L, V>> {
    fn from(mat: Matrix<L, V>) -> Self {
        let mut iter = mat.into_iter();
        use core::iter::{empty, once};
        let iter: Box<dyn Iterator<Item = _>> = match iter.next() {
            None => Box::new(empty()),
            Some(VClause(fv, cl)) => match iter.next() {
                // this is the interesting case
                None if fv.is_empty() => return cl,
                None => Box::new(once(VClause(fv, cl))),
                Some(snd) => Box::new(once(VClause(fv, cl)).chain(once(snd)).chain(iter)),
            },
        };
        Self::from([LitMat::Mat(iter.collect())])
    }
}

impl<L: Display, V: Display> Display for VClause<L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "![")?;
        let mut iter = self.0.iter();
        if let Some(x) = iter.next() {
            write!(f, "{}", x)?;
            for x in iter {
                write!(f, ", {}", x)?;
            }
        }
        write!(f, "]: {}", self.1)
    }
}
