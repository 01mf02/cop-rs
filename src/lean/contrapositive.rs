use super::clause::OClause;
use super::Clause;
use crate::offset::OArgs;
use crate::term::Args;
use crate::{Lit, Offset};
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct Contrapositive<P, C, V> {
    pub head: P,
    pub args: Args<C, V>,
    pub rest: Clause<Lit<P, C, V>>,
    pub vars: Option<V>,
}

pub type OContrapositive<'t, P, C> = Offset<&'t Contrapositive<P, C, usize>>;

impl<P: Display, C: Display, V: Display> Display for Contrapositive<P, C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ∨ {}", self.args, self.rest)
    }
}

impl<'t, P: Display, C: Display> Display for OContrapositive<'t, P, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ∨ {}", self.map(|c| &c.args), self.map(|c| &c.rest))
    }
}

impl<'t, P, C> OContrapositive<'t, P, C> {
    pub fn head(&self) -> &P {
        &self.unwrap().head
    }

    pub fn args(self) -> OArgs<'t, C> {
        self.map(|c| &c.args)
    }

    pub fn rest(self) -> OClause<'t, Lit<P, C, usize>> {
        self.map(|c| &c.rest)
    }
}
