use crate::fof::Dnf;
use crate::term::Fresh;
use crate::{Clause, Lit};
use alloc::vec::Vec;
use hashbrown::HashMap;

impl<P, C, V> Clause<Lit<P, C, V>> {
    pub fn vars(&self) -> impl Iterator<Item = &V> {
        self.iter().flat_map(|lit| lit.vars())
    }
}

impl<P, C, V: Clone + Eq + core::hash::Hash> Clause<Lit<P, C, V>> {
    pub fn fresh_vars<W>(self, map: &mut HashMap<V, W>, st: &mut W::State) -> Clause<Lit<P, C, W>>
    where
        W: Clone + Fresh,
    {
        let iter = self.into_iter();
        let iter = iter.map(|lit| lit.map_args(|args| args.fresh_vars(map, st)));
        Clause(iter.collect())
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
