use alloc::vec::Vec;
use core::fmt::{self, Display};
use core::hash::Hash;
use core::iter::FromIterator;
use hashbrown::HashMap;

/// A database maps (signed) literal heads `P` to contrapositives `CP`.
///
/// This serves to quickly filter the matrix for potential connections.
#[derive(Debug)]
pub struct Db<P, CP>(HashMap<P, Vec<CP>>);

impl<P: Display, CP: Display> Display for Db<P, CP> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        let mut iter = self.0.iter().peekable();
        while let Some((k, v)) = iter.next() {
            write!(f, "{} ↦ {{", k)?;
            let mut citer = v.iter();
            if let Some(contra) = citer.next() {
                write!(f, "{}", contra)?;
                for contra in citer {
                    write!(f, ", {}", contra)?;
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

impl<P: Clone + Eq + Hash, CP> FromIterator<(P, CP)> for Db<P, CP> {
    fn from_iter<I: IntoIterator<Item = (P, CP)>>(iter: I) -> Self {
        let mut db = Self(HashMap::new());
        for (p, cp) in iter {
            db.0.entry(p).or_default().push(cp)
        }
        db
    }
}

impl<P: Eq + Hash, CP> Db<P, CP> {
    /// Obtain the contrapositives for the given predicate.
    pub fn get(&self, p: &P) -> Option<&Vec<CP>> {
        self.0.get(p)
    }
}
