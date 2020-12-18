use super::clause::OClause;
use super::Clause;
use crate::offset::OArgs;
use crate::term::Args;
use crate::{Lit, Offset};
use core::fmt::{self, Display};
use core::hash::Hash;
use core::iter::FromIterator;
use std::collections::HashMap;

pub type DbEntry<P, C, V> = (P, Contrapositive<P, C, V>);

#[derive(Debug)]
pub struct Contrapositive<P, C, V> {
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
    pub fn args(self) -> OArgs<'t, C> {
        self.map(|c| &c.args)
    }

    pub fn rest(self) -> OClause<'t, Lit<P, C, usize>> {
        self.map(|c| &c.rest)
    }
}

#[derive(Debug)]
pub struct Db<P, C, V>(HashMap<P, Vec<Contrapositive<P, C, V>>>);

impl<P: Display, C: Display, V: Display> Display for Db<P, C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        let mut iter = self.0.iter().peekable();
        while let Some((k, v)) = iter.next() {
            write!(f, "{} ↦ {{", k)?;
            let mut citer = v.iter();
            if let Some(contra) = citer.next() {
                write!(f, "{}{}", k, contra)?;
                for contra in citer {
                    write!(f, ", {}{}", k, contra)?;
                }
            }
            write!(f, "}}")?;
            if iter.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl<P: Eq + Hash, C, V> FromIterator<DbEntry<P, C, V>> for Db<P, C, V> {
    fn from_iter<I: IntoIterator<Item = DbEntry<P, C, V>>>(iter: I) -> Self {
        let mut db = Self(HashMap::new());
        for (head, contra) in iter {
            db.0.entry(head).or_default().push(contra)
        }
        db
    }
}

impl<P: Eq + Hash, C, V> Db<P, C, V> {
    pub fn get(&self, p: &P) -> Option<&Vec<Contrapositive<P, C, V>>> {
        self.0.get(p)
    }
}
