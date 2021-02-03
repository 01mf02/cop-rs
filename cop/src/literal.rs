use crate::term::Args;
use crate::App;
use crate::Form;
use core::ops::Neg;

pub type Lit<P, C, V> = App<P, Args<C, V>>;

impl<P, C, V: Ord> Lit<P, C, V> {
    pub fn max_var(&self) -> Option<&V> {
        self.args().max_var()
    }
}

impl<P: Neg<Output = P>, C, V> From<Form<P, C, V>> for Lit<P, C, V> {
    fn from(fm: Form<P, C, V>) -> Self {
        use Form::*;
        match fm {
            Atom(p, args) => Self::new(p, args),
            Neg(a) => match *a {
                Atom(p, args) => Self::new(-p, args),
                _ => panic!("unhandled formula"),
            },
            _ => panic!("unhandled formula"),
        }
    }
}
