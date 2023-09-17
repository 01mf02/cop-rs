//! Nonclausal clauses.
//!
//! Vive l'oxymoron !
use super::Matrix;
use crate::term::Fresh;
use crate::{Lit, LitMat, Offset};
use alloc::{boxed::Box, vec::Vec};
use core::fmt::{self, Display};
use core::hash::Hash;
use hashbrown::HashMap;

/// A clause that can contain literals and matrices.
pub type Clause<L, M> = crate::Clause<LitMat<L, M>>;

/// A clause that additionally registers the bound variables.
#[derive(Debug)]
pub struct VClause<L, V>(pub Vec<V>, pub Clause<L, Matrix<L, V>>);

impl<L, M> Clause<L, M> {
    /// Return the matrices contained in a clause.
    fn mats(&self) -> impl Iterator<Item = &M> {
        self.iter().filter_map(|lm| match lm {
            LitMat::Lit(_) => None,
            LitMat::Mat(m) => Some(m),
        })
    }
}

impl<L, V> Matrix<L, V> {
    /// Return all bound variables of the contained clauses.
    pub fn bound_vars(&self) -> impl Iterator<Item = &V> {
        self.into_iter().flat_map(|c| c.bound_vars())
    }
}

impl<L, V> Clause<L, Matrix<L, V>> {
    /// Return all variables bound somewhere in the clause.
    ///
    /// This does not look at the variables that might occur free in the literals!
    /// Therefore, it makes sense to call this function only on outermost clauses.
    pub fn bound_vars(&self) -> impl Iterator<Item = &V> {
        self.mats().flat_map(|m| m.bound_vars())
    }
}

impl<L, V> VClause<L, V> {
    /// Return all bound variables and variables occurring in the clause.
    pub fn bound_vars(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        Box::new(self.0.iter().chain(self.1.bound_vars()))
    }
}

impl<P, C, V: Clone + Eq + Hash> VClause<Lit<P, C, V>, V> {
    /// Replace all variables in the clause by fresh ones.
    pub fn fresh_vars<W: Clone + Fresh>(
        self,
        map: &mut HashMap<V, W>,
        st: &mut W::State,
    ) -> VClause<Lit<P, C, W>, W> {
        let vars = self.0.into_iter().map(|v| {
            let i = W::fresh(st);
            map.insert(v.clone(), i.clone());
            i
        });
        let vars = vars.collect();

        let cl = self.1.into_iter().map(|lm| match lm {
            LitMat::Lit(l) => LitMat::Lit(l.fresh_vars(map, st)),
            LitMat::Mat(m) => LitMat::Mat(m.into_iter().map(|c| c.fresh_vars(map, st)).collect()),
        });

        VClause(vars, cl.collect())
    }
}

impl<L, V> From<Matrix<L, V>> for Clause<L, Matrix<L, V>> {
    fn from(mat: Matrix<L, V>) -> Self {
        let mut iter = mat.into_iter();
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
        Self::from([LitMat::Mat(iter.collect())])
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

impl<'t, L: 't, V> Display for Offset<&'t VClause<L, V>>
where
    Offset<&'t L>: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.map(|cl| &cl.1).fmt(f)
    }
}
