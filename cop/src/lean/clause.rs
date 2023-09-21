use crate::fof::Dnf;
use crate::{Clause, Lit};
use alloc::vec::Vec;

impl<P, C, V> Clause<Lit<P, C, V>> {
    /// All variables occurring in a clause.
    pub fn vars(&self) -> impl Iterator<Item = &V> {
        self.iter().flat_map(|lit| lit.vars())
    }
}

impl<L: Eq> From<Dnf<L>> for Clause<L> {
    fn from(fm: Dnf<L>) -> Self {
        match fm {
            Dnf::Disj(fms) => {
                let fms = fms.into_iter().rev().map(Self::from);
                fms.reduce(|acc, x| x.union(acc)).unwrap_or_default()
            }
            Dnf::Lit(lit) => Self(Vec::from([lit])),
        }
    }
}
