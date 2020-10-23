use super::database::{Contrapositive, DbEntry};
use crate::{Form, Lit};
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct Clause<C, V>(Vec<Lit<C, V>>);

impl<C: Display, V: Display> Display for Clause<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.iter();
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

impl<C: Eq, V: Eq> Clause<C, V> {
    /// Return whether a clause contains both some literal and its negation.
    fn is_trivial(&self) -> bool {
        self.0
            .iter()
            .any(|l1| self.0.iter().any(|l2| l1.is_neg_of(l2)))
    }

    /// Return the disjunction of two clauses.
    fn union(self, other: Self) -> Self {
        if self.0.is_empty() || other.0.is_empty() {
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

impl<C, V: Ord> Clause<C, V> {
    fn max_var(&self) -> Option<&V> {
        use core::cmp::max;
        self.0.iter().fold(None, |acc, lit| max(lit.max_var(), acc))
    }
}

impl<C: Eq, V: Eq> From<Form<C, V>> for Clause<C, V> {
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

impl<C: Clone, V: Clone + Ord> Clause<C, V> {
    pub fn into_db(self) -> impl Iterator<Item = DbEntry<C, V>> {
        let vars = core::iter::repeat(self.max_var().cloned());
        RestIter::from(self.0).zip(vars).map(|((lit, rest), vars)| {
            let args = lit.args().clone();
            let rest = Clause(rest.into_iter().collect());
            (lit.head().clone(), Contrapositive { args, rest, vars })
        })
    }
}
