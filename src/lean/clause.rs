use crate::fof::Form;
use crate::term::App;

#[derive(Debug)]
pub struct Clause<C, V>(Vec<App<C, V>>);

impl<C: core::ops::Neg<Output = C> + Eq + Clone, V: Eq> Clause<C, V> {
    /// Return whether a clause contains both some literal and its negation.
    fn is_trivial(&self) -> bool {
        self.0.iter().any(|l1| {
            let neg = -l1.c.clone();
            self.0.iter().any(|l2| neg == l2.c && l1.args == l2.args)
        })
    }

    /// Return the disjunction of two clauses.
    fn union(self, other: Self) -> Self {
        if self.0.is_empty() || other.0.is_empty() {
            Self(vec![])
        } else {
            let u = Self(crate::union2(self.0, other.0));
            if u.is_trivial() {
                Self(vec![])
            } else {
                u
            }
        }
    }
}

impl<C: core::ops::Neg<Output = C> + Eq + Clone, V: Eq> From<Form<C, V>> for Clause<C, V> {
    fn from(fm: Form<C, V>) -> Self {
        use core::ops::Neg;
        use Form::*;
        match fm {
            Disj(l, r) => Self::from(*l).union(Self::from(*r)),
            Atom(a) => Self(vec![a]),
            Neg(a) => match *a {
                Atom(a) => Self(vec![a.neg()]),
                _ => panic!("unhandled formula"),
            },
            _ => panic!("unhandled formula"),
        }
    }
}
