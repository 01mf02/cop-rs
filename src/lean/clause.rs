use crate::fof::Form;
use crate::Lit;
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct Clause<C, V>(Vec<Lit<C, V>>);

impl<C: Display, V: Display> Display for Clause<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        let mut iter = self.0.iter();
        if let Some(lit) = iter.next() {
            write!(f, "{}", lit)?;
            for lit in iter {
                write!(f, ", {}", lit)?;
            }
        }
        write!(f, "]")
    }
}

impl<C: Eq, V: Eq> Clause<C, V> {
    /// Return whether a clause contains both some literal and its negation.
    fn is_trivial(&self) -> bool {
        self.0
            .iter()
            .any(|l1| self.0.iter().any(|l2| l1.is_neg_of(l2)))
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

impl<C: Eq, V: Eq> From<Form<C, V>> for Clause<C, V> {
    fn from(fm: Form<C, V>) -> Self {
        use Form::*;
        match fm {
            Disj(l, r) => Self::from(*l).union(Self::from(*r)),
            _ => Self(vec![Lit::from(fm)]),
        }
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
