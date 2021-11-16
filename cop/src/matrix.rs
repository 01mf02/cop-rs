use crate::Offset;
use alloc::vec::{self, Vec};
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct Matrix<C>(Vec<C>);

fn fmt<C: Display>(mut iter: impl Iterator<Item = C>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[")?;
    if let Some(cl) = iter.next() {
        write!(f, "{}", cl)?;
        for cl in iter {
            write!(f, ", {}", cl)?;
        }
    }
    write!(f, "]")
}

impl<C: Display> Display for Matrix<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt(self.0.iter(), f)
    }
}

impl<'t, C: 't> Display for Offset<&'t Matrix<C>>
where
    Offset<&'t C>: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt(self.into_iter(), f)
    }
}

impl<'a, C> IntoIterator for &'a Matrix<C> {
    type Item = &'a C;
    type IntoIter = core::slice::Iter<'a, C>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
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
