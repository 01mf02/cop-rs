use super::Contrapositive;
use crate::fof::Dnf;
use crate::term::Fresh;
use crate::{CtxIter, Lit, Offset};
use alloc::{vec, vec::Vec};
use core::fmt::{self, Display};
use core::ops::Neg;
use hashbrown::HashMap;

#[derive(Debug)]
pub struct Clause<L>(Vec<L>);

pub type OClause<'t, L> = Offset<&'t Clause<L>>;

fn fmt<L: Display>(mut iter: impl Iterator<Item = L>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    if let Some(lit) = iter.next() {
        write!(f, "{}", lit)?;
        for lit in iter {
            write!(f, " ∨ {}", lit)?;
        }
    } else {
        write!(f, "⊥")?
    }
    Ok(())
}

impl<L: Display> Display for Clause<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt(self.into_iter(), f)
    }
}

impl<'t, L> Display for OClause<'t, L>
where
    Offset<&'t L>: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt(self.into_iter(), f)
    }
}

impl<L> Default for Clause<L> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<L> core::ops::Deref for Clause<L> {
    type Target = Vec<L>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<L> core::ops::DerefMut for Clause<L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'a, L> IntoIterator for &'a Clause<L> {
    type Item = &'a L;
    type IntoIter = core::slice::Iter<'a, L>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<L> IntoIterator for Clause<L> {
    type Item = L;
    type IntoIter = vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<L> core::iter::FromIterator<L> for Clause<L> {
    fn from_iter<I: IntoIterator<Item = L>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<L> From<Vec<L>> for Clause<L> {
    fn from(v: Vec<L>) -> Self {
        Self(v)
    }
}

impl<L: Neg<Output = L> + Clone + Eq> Clause<L> {
    /// Return whether a clause contains both some literal and its negation.
    pub fn is_trivial(&self) -> bool {
        self.iter()
            .any(|l1| self.iter().cloned().any(|l2| l1 == &-l2))
    }
}

impl<L: Eq> Clause<L> {
    /// Return the disjunction of two clauses.
    fn union(self, other: Self) -> Self {
        Self(crate::union2(self.0, other.0))
    }
}

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
        let iter = self.0.into_iter();
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
