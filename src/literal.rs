use crate::term::{App, Args};
use crate::Form;
use core::ops::Neg;
use std::fmt::{self, Display};
use std::rc::Rc;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Signed<T>(pub bool, pub T);

impl<T: Clone> Signed<&T> {
    pub fn cloned(self) -> Signed<T> {
        Signed(self.0, self.1.clone())
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

impl<T> Neg for Signed<T> {
    type Output = Self;
    fn neg(self) -> Self {
        Self(!self.0, self.1)
    }
}

impl<T: Eq> Signed<T> {
    pub fn is_neg_of(&self, other: &Self) -> bool {
        self.0 == !other.0 && self.1 == other.1
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Lit<C, V>(Signed<C>, Rc<Args<C, V>>);

impl<C, V> Neg for Lit<C, V> {
    type Output = Self;
    fn neg(self) -> Self {
        Self(-self.0, self.1)
    }
}

impl<C: Display, V: Display> Display for Lit<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

impl<C, V> Lit<C, V> {
    pub fn head(&self) -> &Signed<C> {
        &self.0
    }

    pub fn args(&self) -> &Args<C, V> {
        &self.1
    }
}

impl<C: Eq, V: Eq> Lit<C, V> {
    pub fn is_neg_of(&self, other: &Self) -> bool {
        self.0.is_neg_of(&other.0) && self.1 == other.1
    }
}

impl<C, V: Ord> Lit<C, V> {
    pub fn max_var(&self) -> Option<&V> {
        self.1.max_var()
    }
}

impl<C: Eq, V: Eq> From<Form<C, V>> for Lit<C, V> {
    fn from(fm: Form<C, V>) -> Self {
        use Form::*;
        match fm {
            Atom(App { c, args }) => Self(Signed(true, c), Rc::new(args)),
            Neg(a) => match *a {
                Atom(App { c, args }) => Self(Signed(false, c), Rc::new(args)),
                _ => panic!("unhandled formula"),
            },
            _ => panic!("unhandled formula"),
        }
    }
}
