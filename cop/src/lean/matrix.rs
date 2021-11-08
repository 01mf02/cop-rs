use super::Contrapositive;
use crate::fof::Cnf;
use crate::{Clause, Lit};

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

impl<P: Clone, C: Clone, V: Clone + Ord> Matrix<Lit<P, C, V>> {
    pub fn contrapositives(&self) -> impl Iterator<Item = Contrapositive<Lit<P, C, V>, V>> {
        self.into_iter().flat_map(|cl| {
            let max_var = cl.max_var();
            cl.contrapositives().map(move |cp| Contrapositive {
                lit: cp.lit,
                rest: cp.rest.into_iter().cloned().collect(),
                vars: max_var.cloned(),
            })
        })
    }
}
