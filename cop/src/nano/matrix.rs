use super::clause::{Clause, VClause};
use super::LitMat;
use crate::fof::{Forall, Nnf, OpA};
use alloc::vec::Vec;

pub type Matrix<L, V> = crate::Matrix<VClause<L, V>>;

impl<L, V: Clone> From<Nnf<L, V, Forall>> for Matrix<L, V> {
    fn from(fm: Nnf<L, V, Forall>) -> Self {
        Self::from_nnf(fm, &mut Vec::new())
    }
}

impl<L, V: Clone> Matrix<L, V> {
    fn from_nnf(fm: Nnf<L, V, Forall>, fv: &mut Vec<V>) -> Self {
        use core::iter::once;
        match fm {
            Nnf::Lit(l) => once(VClause(fv.clone(), Clause::from([LitMat::Lit(l)]))).collect(),
            Nnf::BinA(OpA::Conj, fms) => fms
                .into_iter()
                .flat_map(|fm| Self::from_nnf(fm, fv))
                .collect(),
            Nnf::BinA(OpA::Disj, fms) => {
                let mut cl = Clause::default();
                fms.into_iter()
                    .for_each(|x| cl.append(&mut Clause::from(Self::from(x))));
                once(VClause(fv.clone(), cl)).collect()
            }
            Nnf::Quant(Forall, v, fm) => {
                fv.push(v);
                let mat = Self::from_nnf(*fm, fv);
                fv.pop();
                mat
            }
        }
    }
}
