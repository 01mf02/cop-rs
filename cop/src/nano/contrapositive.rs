use super::clause::{Clause, VClause};
use super::matrix;
use super::LitMat;
use crate::CtxIter;
use crate::{Lit, Matrix};
use alloc::vec::Vec;
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct PreCp<'a, L, V> {
    lit: &'a L,
    /// smallest clause originally containing `lit`, but without `lit`
    beta_cla: BetaClause<'a, L, V>,
    /// all clauses and matrices originally containing `lit`, largest first
    ctx: Vec<Ctx<'a, L, V>>,
}

#[derive(Clone)]
pub struct CtxBla<'a, I, C> {
    current: I,
    ctx: &'a [C],
}

impl<'a, I, C> CtxBla<'a, I, C>
where
    &'a C: IntoIterator<IntoIter = I>,
{
    fn next(&mut self) -> bool {
        if let Some(last) = self.ctx.last() {
            self.current = last.into_iter();
            self.ctx = &self.ctx[..self.ctx.len() - 1];
            true
        } else {
            false
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

impl<P, C, V: Ord> matrix::Matrix<Lit<P, C, V>, V> {
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
        use alloc::boxed::Box;
        let elems: Vec<_> = self.1.iter().collect();
        CtxIter::from(elems).flat_map(move |(lm, beta_cla)| match lm {
            LitMat::Lit(lit) => Box::new(core::iter::once(PreCp {
                lit,
                beta_cla: beta_cla.iter().map(|lm| lm.as_ref()).collect(),
                ctx: ctx.clone(),
            })),
            LitMat::Mat(full_mat) => {
                let ctx2 = ctx.clone();
                let matv: Vec<_> = full_mat.into_iter().collect();
                Box::new(CtxIter::from(matv).flat_map(move |(cl, rest)| {
                    let beta_mat: Matrix<_> = rest.into_iter().collect();
                    let mut ctx = ctx2.clone();
                    ctx.push(Ctx {
                        full_cla: self,
                        beta_cla: beta_cla.iter().map(|lm| lm.as_ref()).collect(),
                        full_mat,
                        beta_mat,
                    });
                    cl.pre_cps(ctx)
                })) as Box<dyn Iterator<Item = _>>
            }
        })
    }
}
