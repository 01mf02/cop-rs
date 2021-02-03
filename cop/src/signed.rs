use core::fmt::{self, Display};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Signed<T>(pub bool, pub T);

impl<T: Clone> Signed<&T> {
    pub fn cloned(self) -> Signed<T> {
        Signed(self.0, self.1.clone())
    }
}

impl<T> Signed<T> {
    pub fn is_sign_positive(&self) -> bool {
        self.0
    }

    pub fn is_sign_negative(&self) -> bool {
        !self.0
    }
}

impl<T: Display> Display for Signed<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 {
            write!(f, "{}", self.1)
        } else {
            write!(f, "¬ {}", self.1)
        }
    }
}

impl<T> From<T> for Signed<T> {
    fn from(x: T) -> Self {
        Self(true, x)
    }
}

impl<T> core::ops::Neg for Signed<T> {
    type Output = Self;
    fn neg(self) -> Self {
        Self(!self.0, self.1)
    }
}
