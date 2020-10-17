use super::clause::Clause;
use super::database::DbEntry;
use crate::fof::Form;
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct Matrix<C, V>(Vec<Clause<C, V>>);

impl<C: Display, V: Display> Display for Matrix<C, V> {
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

impl<C, V> Matrix<C, V> {
    /// Return the conjunction of two matrices.
    fn union(mut self, mut other: Self) -> Self {
        self.0.append(&mut other.0);
        self
    }
}

impl<C: Eq, V: Eq> From<Form<C, V>> for Matrix<C, V> {
    fn from(fm: Form<C, V>) -> Self {
        match fm {
            Form::Conj(l, r) => Self::from(*l).union(Self::from(*r)),
            _ => Self(vec![Clause::from(fm)]),
        }
    }
}

impl<C: Clone, V: Clone + Ord> Matrix<C, V> {
    pub fn into_db(self) -> impl Iterator<Item = DbEntry<C, V>> {
        self.0.into_iter().flat_map(|cl| cl.into_db())
    }
}
