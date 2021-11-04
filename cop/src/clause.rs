use crate::Offset;
use alloc::{vec, vec::Vec};
use core::fmt::{self, Display};
use core::ops::Neg;

// TODO: it would be nice to get rid of `Clone` here, which is currently
// necessary due to the nonclausal extension clause generation
#[derive(Clone, Debug)]
pub struct Clause<L>(pub Vec<L>);

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
