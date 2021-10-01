use super::{Clause, Contrapositive};
use crate::fof::Cnf;
use crate::Lit;
use alloc::{vec, vec::Vec};
use core::fmt::{self, Display};

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

impl<L: Eq> From<Cnf<L>> for Matrix<L> {
    fn from(fm: Cnf<L>) -> Self {
        match fm {
            Cnf::Conj(fms) => fms
                .into_iter()
                .flat_map(|fm| Self::from(fm).into_iter())
                .collect(),
            Cnf::Disj(disj) => Self(Vec::from([Clause::from(disj)])),
        }
    }
}

impl<L> IntoIterator for Matrix<L> {
    type Item = Clause<L>;
    type IntoIter = vec::IntoIter<Self::Item>;

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
    pub fn contrapositives(self) -> impl Iterator<Item = Contrapositive<P, C, V>> {
        self.0.into_iter().flat_map(|cl| cl.contrapositives())
    }
}
