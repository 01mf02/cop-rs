use super::Contrapositive;
use crate::fof::Cnf;
use crate::{Clause, Lit};

/// A matrix of clauses.
pub type Matrix<L> = crate::Matrix<Clause<L>>;

impl<L: Eq> From<Cnf<L>> for Matrix<L> {
    fn from(fm: Cnf<L>) -> Self {
        match fm {
            Cnf::Conj(fms) => fms
                .into_iter()
                .flat_map(|fm| Self::from(fm).into_iter())
                .collect(),
            Cnf::Disj(disj) => core::iter::once(Clause::from(disj)).collect(),
        }
    }
}

impl<P, C, V: Clone + Ord> Matrix<Lit<P, C, V>> {
    /// Decompose a matrix into contrapositives.
    pub fn contrapositives(&self) -> impl Iterator<Item = Contrapositive<Lit<P, C, V>, V>> {
        self.into_iter().flat_map(|cl| {
            let max_var = cl.vars().max();
            cl.contrapositives().map(move |contra| Contrapositive {
                contra,
                vars: max_var.cloned(),
            })
        })
    }
}
