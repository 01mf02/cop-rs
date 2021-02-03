use alloc::{vec, vec::Vec};
use core::fmt::{self, Display};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Args<T>(Vec<T>);

impl<T: Display> Display for Args<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.iter();
        if let Some(arg) = iter.next() {
            write!(f, "({}", arg)?;
            for arg in iter {
                write!(f, ", {}", arg)?;
            }
            write!(f, ")")?
        }
        Ok(())
    }
}

impl<'a, T> IntoIterator for &'a Args<T> {
    type Item = &'a T;
    type IntoIter = core::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<T> IntoIterator for Args<T> {
    type Item = T;
    type IntoIter = vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> core::iter::FromIterator<T> for Args<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<T> core::ops::Deref for Args<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
