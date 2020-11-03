use crate::term::Args;
use crate::Form;
use core::fmt::{self, Display};
use core::ops::Neg;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Lit<P, A>(P, A);

impl<P: Neg<Output = P>, A> Neg for Lit<P, A> {
    type Output = Self;
    fn neg(self) -> Self {
        Self(-self.0, self.1)
    }
}

impl<P: Display, A: Display> Display for Lit<P, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.0, self.1)
    }
}

impl<P, A> Lit<P, A> {
    pub fn head(&self) -> &P {
        &self.0
    }

    pub fn args(&self) -> &A {
        &self.1
    }

    pub fn map_args<B>(self, f: impl FnOnce(A) -> B) -> Lit<P, B> {
        Lit(self.0, f(self.1))
    }
}

impl<P, C, V: Ord> Lit<P, Args<C, V>> {
    pub fn max_var(&self) -> Option<&V> {
        self.args().max_var()
    }
}

impl<P: From<C> + Neg<Output = P>, C, V> From<Form<C, V>> for Lit<P, Args<C, V>> {
    fn from(fm: Form<C, V>) -> Self {
        use Form::*;
        match fm {
            Atom(p, args) => Self(P::from(p), args),
            Neg(a) => match *a {
                Atom(p, args) => Self(-P::from(p), args),
                _ => panic!("unhandled formula"),
            },
            _ => panic!("unhandled formula"),
        }
    }
}
