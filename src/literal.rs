use crate::term::{App, Term};
use crate::Form;
use core::ops::Neg;
use std::fmt::{self, Display};

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

pub type Lit<C, V> = Signed<App<C, V>>;

impl<C, V> Lit<C, V> {
    pub fn head(&self) -> Signed<&C> {
        Signed(self.0, &self.1.c)
    }

    pub fn args(&self) -> &Vec<Term<C, V>> {
        &self.1.args
    }
}

impl<C: Eq, V: Eq> Lit<C, V> {
    pub fn is_neg_of(&self, other: &Self) -> bool {
        self.0 != other.0 && self.1 == other.1
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
            Atom(a) => Self(true, a),
            Neg(a) => match *a {
                Atom(a) => Self(false, a),
                _ => panic!("unhandled formula"),
            },
            _ => panic!("unhandled formula"),
        }
    }
}
