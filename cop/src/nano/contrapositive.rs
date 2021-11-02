use super::clause::{Clause, VClause};
use super::matrix;
use crate::CtxIter;
use crate::{Lit, Matrix};
use alloc::vec::Vec;
use core::fmt::{self, Display};

pub struct PreCp<'a, L, V> {
    lit: &'a L,
    beta_cla: BetaClause<'a, L, V>,
    ctx: Vec<Ctx<'a, L, V>>,
}

pub struct VContrapositive<'a, L, V> {
    contra: PreCp<'a, L, V>,
    // groundness of beta_cla \cup args
    ground: bool,
    // maximal variable of ctx[0].full_cla (the largest clause containing lit) or
    // (if ctx empty) beta_cla \cup args
    offset: Option<&'a V>,
}

impl<'a, P: Clone, C, V> VContrapositive<'a, Lit<P, C, V>, V> {
    pub fn db_entry(self) -> (P, Self) {
        (self.contra.lit.head().clone(), self)
    }
}

impl<'a, L: Display, V: Display> Display for VContrapositive<'a, L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.contra.fmt(f)
    }
}

impl<'a, L: Display, V: Display> Display for PreCp<'a, L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}, {}", self.lit, self.beta_cla)?;
        self.ctx.iter().try_for_each(|c| write!(f, ", {}", c))
    }
}

type BetaClause<'a, L, V> = Clause<&'a L, &'a matrix::Matrix<L, V>>;

pub struct Ctx<'a, L, V> {
    full_cla: &'a VClause<L, V>,
    beta_cla: BetaClause<'a, L, V>,
    full_mat: &'a Matrix<VClause<L, V>>,
    beta_mat: Matrix<&'a VClause<L, V>>,
}

impl<'a, L: Display, V: Display> Display for Ctx<'a, L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "C{}, M{}", self.beta_cla, self.beta_mat)
    }
}

impl<'a, L, V> Clone for Ctx<'a, L, V> {
    fn clone(&self) -> Self {
        Self {
            full_cla: self.full_cla,
            beta_cla: self.beta_cla.clone(),
            full_mat: self.full_mat,
            beta_mat: self.beta_mat.clone(),
        }
    }
}

impl<P, C, V: Ord> super::Matrix<Lit<P, C, V>, V> {
    pub fn pre_cps(&self) -> impl Iterator<Item = VContrapositive<Lit<P, C, V>, V>> {
        self.into_iter().flat_map(|cl| {
            let offset = cl.max_var();
            cl.pre_cps(Vec::new()).map(move |contra| VContrapositive {
                ground: contra.lit.is_ground() && contra.beta_cla.is_ground(),
                offset,
                contra,
            })
        })
    }
}

impl<L, V> VClause<L, V> {
    fn pre_cps<'a>(&'a self, ctx: Vec<Ctx<'a, L, V>>) -> impl Iterator<Item = PreCp<'a, L, V>> {
        let lits: Vec<_> = self.1.lits.iter().collect();
        let ctx1 = ctx.clone();
        let lits = CtxIter::from(lits).map(move |(lit, rest)| {
            let beta_cla = Clause {
                lits: rest.into_iter().collect(),
                mats: self.1.mats.iter().collect(),
            };
            PreCp {
                lit,
                beta_cla,
                ctx: ctx1.clone(),
            }
        });

        let mats: Vec<_> = self.1.mats.iter().collect();
        let mats = CtxIter::from(mats).flat_map(move |(full_mat, rest)| {
            let ctx2 = ctx.clone();
            let beta_cla = Clause {
                lits: self.1.lits.iter().collect(),
                mats: rest.into_iter().collect(),
            };
            let matv: Vec<_> = full_mat.into_iter().collect();
            CtxIter::from(matv).flat_map(move |(cl, rest)| {
                let beta_mat: Matrix<_> = rest.into_iter().collect();
                let mut ctx = ctx2.clone();
                ctx.push(Ctx {
                    full_cla: self,
                    beta_cla: beta_cla.clone(),
                    full_mat,
                    beta_mat,
                });
                use alloc::boxed::Box;
                Box::new(cl.pre_cps(ctx)) as Box<dyn Iterator<Item = _>>
            })
        });

        lits.chain(mats)
    }
}
