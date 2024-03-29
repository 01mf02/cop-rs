use crate::Offset;
use alloc::{vec, vec::Vec};
use core::fmt::{self, Display};
use core::ops::Neg;

/// Clause of literals `L`.
#[derive(Debug)]
pub struct Clause<L>(pub Vec<L>);

/// Offset clause.
pub type OClause<'t, L> = Offset<&'t Clause<L>>;

/// A clause that is decomposed into a literal and the rest clause.
#[derive(Debug)]
pub struct Contrapositive<L, LM = L> {
    /// literal
    pub lit: L,
    /// the position of the literal in the original clause
    pub pos: usize,
    /// the remaining clause
    pub rest: Clause<LM>,
}

impl<L> Clause<L> {
    /// Decompose a clause into all its contrapositives.
    pub fn contrapositives(&self) -> impl Iterator<Item = Contrapositive<&L>> {
        self.iter().enumerate().map(move |(i, x)| Contrapositive {
            lit: x,
            pos: i,
            rest: self
                .iter()
                .enumerate()
                .filter_map(|(j, y)| (i != j).then_some(y))
                .collect(),
        })
    }
}

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
        fmt(self.iter(), f)
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

impl<L: Display, LM: Display> Display for Contrapositive<L, LM> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ∨ {}", self.lit, self.rest)
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

impl<L, const N: usize> From<[L; N]> for Clause<L> {
    fn from(s: [L; N]) -> Self {
        Self(s.into())
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
    pub fn union(self, other: Self) -> Self {
        Self(crate::union2(self.0, other.0))
    }
}
