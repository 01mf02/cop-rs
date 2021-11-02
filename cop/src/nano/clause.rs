use super::Matrix;
use crate::Lit;
use alloc::vec::Vec;
use core::fmt::{self, Display};

#[derive(Clone)]
pub struct Clause<L, M> {
    pub lits: Vec<L>,
    pub mats: Vec<M>,
}

pub struct VClause<L, V>(pub Vec<V>, pub Clause<L, Matrix<L, V>>);

impl<L, M> From<Vec<L>> for Clause<L, M> {
    fn from(lits: Vec<L>) -> Self {
        let mats = Vec::new();
        Self { lits, mats }
    }
}

impl<L, M> Clause<L, M> {
    pub fn new() -> Self {
        Self {
            lits: Vec::new(),
            mats: Vec::new(),
        }
    }

    pub fn append(&mut self, other: &mut Clause<L, M>) {
        self.lits.append(&mut other.lits);
        self.mats.append(&mut other.mats);
    }
}

impl<P, C, V, M> Clause<Lit<P, C, V>, M> {
    pub fn is_ground(&self) -> bool {
        self.mats.is_empty() && self.lits.iter().all(|lit| lit.is_ground())
    }
}

impl<L, V: Ord> VClause<L, V> {
    /// Return the maximal variable bound somewhere in the clause.
    ///
    /// This does not look at the variables that might occur free in the literals!
    /// Therefore, it makes sense to call this function only on outermost clauses.
    pub fn max_var(&self) -> Option<&V> {
        let mats = self.1.mats.iter();
        let mats_max = mats
            .map(|m| m.into_iter().map(|c| c.max_var()).max().flatten())
            .max()
            .flatten();
        core::cmp::max(self.0.iter().max(), mats_max)
    }
}

impl<L, V> From<Matrix<L, V>> for Clause<L, Matrix<L, V>> {
    fn from(mat: Matrix<L, V>) -> Self {
        let mut iter = mat.into_iter();
        use alloc::boxed::Box;
        use core::iter::{empty, once};
        let iter: Box<dyn Iterator<Item = _>> = match iter.next() {
            None => Box::new(empty()),
            Some(VClause(fv, cl)) => match iter.next() {
                // this is the interesting case
                None if fv.is_empty() => return cl,
                None => Box::new(once(VClause(fv, cl))),
                Some(snd) => Box::new(once(VClause(fv, cl)).chain(once(snd)).chain(iter)),
            },
        };
        Clause {
            lits: Vec::new(),
            mats: Vec::from([iter.collect()]),
        }
    }
}

impl<L: Display, V: Display> Display for Clause<L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        use alloc::boxed::Box;
        let lits = self.lits.iter().map(|x| Box::new(x) as Box<dyn Display>);
        let mats = self.mats.iter().map(|x| Box::new(x) as Box<dyn Display>);
        let mut iter = lits.chain(mats);
        if let Some(lm) = iter.next() {
            write!(f, "{}", lm)?;
            for lm in iter {
                write!(f, ", {}", lm)?;
            }
        }
        write!(f, "]")
    }
}

impl<L: Display, V: Display> Display for VClause<L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "![")?;
        let mut iter = self.0.iter();
        if let Some(x) = iter.next() {
            write!(f, "{}", x)?;
            for x in iter {
                write!(f, ", {}", x)?;
            }
        }
        write!(f, "]: {}", self.1)
    }
}
