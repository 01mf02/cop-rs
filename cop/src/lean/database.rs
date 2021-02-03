use super::Contrapositive;
use alloc::vec::Vec;
use core::fmt::{self, Display};
use core::hash::Hash;
use core::iter::FromIterator;
use hashbrown::HashMap;

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

impl<P: Clone + Eq + Hash, C, V> FromIterator<Contrapositive<P, C, V>> for Db<P, C, V> {
    fn from_iter<I: IntoIterator<Item = Contrapositive<P, C, V>>>(iter: I) -> Self {
        let mut db = Self(HashMap::new());
        for cp in iter {
            db.0.entry(cp.head.clone()).or_default().push(cp)
        }
        db
    }
}

impl<P: Eq + Hash, C, V> Db<P, C, V> {
    pub fn get(&self, p: &P) -> Option<&Vec<Contrapositive<P, C, V>>> {
        self.0.get(p)
    }
}
