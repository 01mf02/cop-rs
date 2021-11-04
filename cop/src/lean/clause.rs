use super::Contrapositive;
use crate::fof::Dnf;
use crate::term::Fresh;
use crate::{Clause, CtxIter, Lit};
use alloc::vec::Vec;
use hashbrown::HashMap;

impl<P, C, V: Ord> Clause<Lit<P, C, V>> {
    fn max_var(&self) -> Option<&V> {
        self.iter().map(|lit| lit.max_var()).max().flatten()
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

impl<P: Clone, C: Clone, V: Clone + Ord> Clause<Lit<P, C, V>> {
    pub fn contrapositives(self) -> impl Iterator<Item = Contrapositive<P, C, V>> {
        let vars = core::iter::repeat(self.max_var().cloned());
        CtxIter::from(self.0).zip(vars).map(|((lit, rest), vars)| {
            let rest = Clause(rest.into_iter().collect());
            Contrapositive { lit, rest, vars }
        })
    }
}
