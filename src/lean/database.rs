use super::Clause;
use crate::literal::{Lit, Signed};
use crate::term::Args;
use core::fmt::{self, Display};
use core::hash::Hash;
use core::iter::FromIterator;
use std::collections::HashMap;

pub type DbEntry<C, V> = (Signed<C>, Contrapositive<C, V>);

#[derive(Debug)]
pub struct Contrapositive<C, V> {
    pub args: Args<C, V>,
    pub rest: Clause<C, V>,
    pub vars: Option<V>,
}

impl<C: Display, V: Display> Display for Contrapositive<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ∨ {}", self.args, self.rest)
    }
}

#[derive(Debug)]
pub struct Db<C, V>(HashMap<Signed<C>, Vec<Contrapositive<C, V>>>);

impl<C: Display, V: Display> Display for Db<C, V> {
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

impl<C: Eq + Hash, V> FromIterator<DbEntry<C, V>> for Db<C, V> {
    fn from_iter<I: IntoIterator<Item = DbEntry<C, V>>>(iter: I) -> Self {
        let mut db = Self(HashMap::new());
        for (head, contra) in iter {
            db.0.entry(head).or_default().push(contra)
        }
        db
    }
}

impl<C: Clone + Eq + Hash, V> Db<C, V> {
    pub fn get(&self, lit: &Lit<C, V>) -> Option<&Vec<Contrapositive<C, V>>> {
        self.0.get(lit.head())
    }
}
