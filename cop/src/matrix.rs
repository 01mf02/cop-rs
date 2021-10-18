use alloc::vec::{self, Vec};
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct Matrix<C>(Vec<C>);

impl<C: Display> Display for Matrix<C> {
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

impl<C> IntoIterator for Matrix<C> {
    type Item = C;
    type IntoIter = vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<C> core::iter::FromIterator<C> for Matrix<C> {
    fn from_iter<I: IntoIterator<Item = C>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<C> core::ops::Deref for Matrix<C> {
    type Target = Vec<C>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<C> core::ops::DerefMut for Matrix<C> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
