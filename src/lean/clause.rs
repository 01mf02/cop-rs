use super::database::{Contrapositive, DbEntry};
use crate::term::{Args, Fresh};
use crate::{Form, Lit};
use core::fmt::{self, Display};
use core::ops::Neg;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Clause<L>(Vec<L>);

impl<L: Display> Display for Clause<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.into_iter();
        if let Some(lit) = iter.next() {
            write!(f, "{}", lit)?;
            for lit in iter {
                write!(f, "∨ {}", lit)?;
            }
        } else {
            write!(f, "⊥")?
        }
        Ok(())
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

impl<L: Neg<Output = L> + Clone + Eq> Clause<L> {
    /// Return whether a clause contains both some literal and its negation.
    fn is_trivial(&self) -> bool {
        self.iter()
            .any(|l1| self.iter().cloned().any(|l2| l1 == &-l2))
    }

    /// Return the disjunction of two clauses.
    fn union(self, other: Self) -> Self {
        if self.is_empty() || other.is_empty() {
            Self(vec![])
        } else {
            let u = Self(crate::union2(self.0, other.0));
            if u.is_trivial() {
                Self(vec![])
            } else {
                u
            }
        }
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

impl<P, C, V> From<Form<C, V>> for Clause<Lit<P, C, V>>
where
    P: Clone + Eq + From<C> + Neg<Output = P>,
    C: Clone + Eq,
    V: Clone + Eq,
{
    fn from(fm: Form<C, V>) -> Self {
        use Form::*;
        match fm {
            Disj(l, r) => Self::from(*l).union(Self::from(*r)),
            _ => Self(vec![Lit::from(fm)]),
        }
    }
}

#[derive(Clone)]
struct RestIter<T> {
    left: Vec<T>,
    right: Vec<T>,
}

impl<T> RestIter<T> {
    fn into_iter(self) -> impl Iterator<Item = T> {
        let right = self.right.into_iter().rev().skip(1);
        self.left.into_iter().chain(right)
    }
}

impl<T> From<Vec<T>> for RestIter<T> {
    fn from(v: Vec<T>) -> Self {
        Self {
            left: v,
            right: vec![],
        }
    }
}

impl<T: Clone> Iterator for RestIter<T> {
    type Item = (T, RestIter<T>);
    fn next(&mut self) -> Option<Self::Item> {
        let mid = self.left.pop()?;
        self.right.push(mid.clone());
        Some((mid, self.clone()))
    }
}

impl<P: Clone, C: Clone, V: Clone + Ord> Clause<Lit<P, C, V>> {
    pub fn into_db(self) -> impl Iterator<Item = DbEntry<P, C, V>> {
        let vars = core::iter::repeat(self.max_var().cloned());
        RestIter::from(self.0).zip(vars).map(|((lit, rest), vars)| {
            let args = lit.args().clone();
            let rest = Clause(rest.into_iter().collect());
            (lit.head().clone(), Contrapositive { args, rest, vars })
        })
    }
}
