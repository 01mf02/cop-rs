use crate::term::Args;
use crate::App;
use crate::Form;
use core::fmt::{self, Display};
use core::ops::Neg;

pub type Lit<P, C, V> = App<P, Args<C, V>>;

impl<P, C, V: Ord> Lit<P, C, V> {
    pub fn max_var(&self) -> Option<&V> {
        self.args().max_var()
    }
}

impl<P: From<C> + Neg<Output = P>, C, V> From<Form<C, V>> for Lit<P, C, V> {
    fn from(fm: Form<C, V>) -> Self {
        use Form::*;
        match fm {
            Atom(p, args) => Self::new(P::from(p), args),
            Neg(a) => match *a {
                Atom(p, args) => Self::new(-P::from(p), args),
                _ => panic!("unhandled formula"),
            },
            _ => panic!("unhandled formula"),
        }
    }
}
