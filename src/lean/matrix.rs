use super::clause::Clause;
use super::database::DbEntry;
use crate::fof::Form;
use crate::Lit;
use core::fmt::{self, Display};
use core::ops::Neg;

#[derive(Debug)]
pub struct Matrix<L>(Vec<Clause<L>>);

impl<L: Display> Display for Matrix<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        let mut iter = self.0.iter();
        if let Some(cl) = iter.next() {
            write!(f, "{}", cl)?;
            for cl in iter {
                write!(f, ", {}", cl)?;
            }
        }
        write!(f, "]")
    }
}

impl<L> Matrix<L> {
    /// Return the conjunction of two matrices.
    fn union(mut self, mut other: Self) -> Self {
        self.0.append(&mut other.0);
        self
    }
}

impl<P, C, V> From<Form<C, V>> for Matrix<Lit<P, C, V>>
where
    P: Clone + Eq + From<C> + Neg<Output = P>,
    C: Clone + Eq,
    V: Clone + Eq,
{
    fn from(fm: Form<C, V>) -> Self {
        match fm {
            Form::Conj(l, r) => Self::from(*l).union(Self::from(*r)),
            _ => Self(vec![Clause::from(fm)]),
        }
    }
}

impl<L> IntoIterator for Matrix<L> {
    type Item = Clause<L>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<L> core::iter::FromIterator<Clause<L>> for Matrix<L> {
    fn from_iter<I: IntoIterator<Item = Clause<L>>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<L> core::ops::Deref for Matrix<L> {
    type Target = Vec<Clause<L>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<L> core::ops::DerefMut for Matrix<L> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<P: Clone, C: Clone, V: Clone + Ord> Matrix<Lit<P, C, V>> {
    pub fn into_db(self) -> impl Iterator<Item = DbEntry<P, C, V>> {
        self.0.into_iter().flat_map(|cl| cl.into_db())
    }
}
