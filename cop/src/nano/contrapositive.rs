use super::clause::{Clause, VClause};
use super::matrix;
use crate::CtxIter;
use crate::Matrix;
use alloc::vec::Vec;

type PreCp<'a, L, V> = (&'a L, BetaClause<'a, L, V>, Ctx<'a, L, V>);

type Ctx<'a, L, V> = Vec<(BetaClause<'a, L, V>, BetaMatrix<'a, L, V>)>;

type BetaClause<'a, L, V> = Clause<&'a L, &'a matrix::Matrix<L, V>>;
type BetaMatrix<'a, L, V> = Matrix<&'a VClause<L, V>>;

impl<L, V> super::Matrix<L, V> {
    pub fn pre_cps<'a>(&'a self) -> impl Iterator<Item = PreCp<'a, L, V>> {
        self.into_iter().flat_map(|cl| cl.1.pre_cps(Vec::new()))
    }
}

impl<L, V> Clause<L, super::Matrix<L, V>> {
    fn pre_cps<'a>(&'a self, ctx: Ctx<'a, L, V>) -> impl Iterator<Item = PreCp<'a, L, V>> {
        use alloc::boxed::Box;
        let lits: Vec<_> = self.lits.iter().collect();
        let ctx1 = ctx.clone();
        let lits = CtxIter::from(lits).map(move |(lit, rest)| {
            let beta_cla = Clause {
                lits: rest.into_iter().collect(),
                mats: self.mats.iter().collect(),
            };
            (lit, beta_cla, ctx1.clone())
        });

        let mats: Vec<_> = self.mats.iter().collect();
        let mats = CtxIter::from(mats).flat_map(move |(mat, rest)| {
            let ctx2 = ctx.clone();
            let beta_cla = Clause {
                lits: self.lits.iter().collect(),
                mats: rest.into_iter().collect(),
            };
            let mat: Vec<_> = mat.into_iter().collect();
            CtxIter::from(mat).flat_map(move |(cl, rest)| {
                let beta_mat: Matrix<_> = rest.into_iter().collect();
                let mut ctx = ctx2.clone();
                ctx.push((beta_cla.clone(), beta_mat));
                Box::new(cl.1.pre_cps(ctx)) as Box<dyn Iterator<Item = _>>
            })
        });

        lits.chain(mats)
    }
}
