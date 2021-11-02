use super::clause::OClause;
use super::Clause;
use crate::offset::OArgs;
use crate::{Lit, Offset};
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct Contrapositive<P, C, V> {
    pub lit: Lit<P, C, V>,
    pub rest: Clause<Lit<P, C, V>>,
    pub vars: Option<V>,
}

impl<P: Clone, C, V> Contrapositive<P, C, V> {
    pub fn db_entry(self) -> (P, Self) {
        (self.lit.head().clone(), self)
    }
}

pub type OContrapositive<'t, P, C> = Offset<&'t Contrapositive<P, C, usize>>;

impl<P: Display, C: Display, V: Display> Display for Contrapositive<P, C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ∨ {}", self.lit, self.rest)
    }
}

impl<'t, P: Display, C: Display> Display for OContrapositive<'t, P, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{} ∨ {}", self.head(), self.args(), self.rest())
    }
}

impl<'t, P, C> OContrapositive<'t, P, C> {
    pub fn head(&self) -> &P {
        self.unwrap().lit.head()
    }

    pub fn args(self) -> OArgs<'t, C> {
        self.map(|c| c.lit.args())
    }

    pub fn rest(self) -> OClause<'t, Lit<P, C, usize>> {
        self.map(|c| &c.rest)
    }
}
