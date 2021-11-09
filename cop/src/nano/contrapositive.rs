use super::clause::VClause;
use super::matrix;
use crate::{Lit, LitMat, Matrix};
use alloc::vec::Vec;
use core::fmt::{self, Display};

#[derive(Debug)]
pub struct PreCp<'a, L, V> {
    contra: crate::clause::Contrapositive<&'a L, &'a LitMat<L, matrix::Matrix<L, V>>>,
    /// all clauses and matrices originally containing `lit`, largest first
    ctx: Vec<Ctx<'a, L, V>>,
    /// groundness of beta_cla \cup args
    ground: bool,
    /// maximal variable of ctx[0].full_cla (the largest clause containing lit) or
    /// (if ctx empty) beta_cla \cup args
    offset: Option<V>,
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

impl<'a, P: Clone, C, V> PreCp<'a, Lit<P, C, V>, V> {
    pub fn db_entry(self) -> (P, Self) {
        (self.contra.lit.head().clone(), self)
    }
}

impl<'a, L: Display, V: Display> Display for PreCp<'a, L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.contra)?;
        self.ctx.iter().try_for_each(|c| write!(f, ", {}", c))
    }
}

#[derive(Debug)]
pub struct Ctx<'a, L, V> {
    full_cla: &'a VClause<L, V>,
    beta_cla: crate::Clause<&'a LitMat<L, matrix::Matrix<L, V>>>,
    full_mat: &'a Matrix<VClause<L, V>>,
}

impl<'a, L: Display, V: Display> Display for Ctx<'a, L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.beta_cla)
    }
}

impl<'a, L, V> Clone for Ctx<'a, L, V> {
    fn clone(&self) -> Self {
        Self {
            full_cla: self.full_cla,
            beta_cla: self.beta_cla.iter().cloned().collect(),
            full_mat: self.full_mat,
        }
    }
}

impl<P, C, V: Clone + Ord> matrix::Matrix<Lit<P, C, V>, V> {
    pub fn pre_cps(&self) -> impl Iterator<Item = PreCp<Lit<P, C, V>, V>> {
        self.into_iter().flat_map(|cl| {
            let offset = cl.max_var();
            cl.pre_cps(Vec::new()).map(move |contra| {
                let ground = contra.contra.lit.is_ground()
                    && contra.contra.rest.iter().all(|lm| lm.is_ground());
                PreCp {
                    ground,
                    offset: offset.cloned(),
                    ..contra
                }
            })
        })
    }
}

impl<L, V> VClause<L, V> {
    fn pre_cps<'a>(&'a self, ctx: Vec<Ctx<'a, L, V>>) -> impl Iterator<Item = PreCp<'a, L, V>> {
        use alloc::boxed::Box;
        self.1.contrapositives().flat_map(move |cp| match cp.lit {
            LitMat::Lit(lit) => Box::new(core::iter::once(PreCp {
                contra: crate::clause::Contrapositive { lit, rest: cp.rest },
                ctx: ctx.clone(),
                // these values are just placeholders that are overwritten later
                ground: true,
                offset: None,
            })),
            LitMat::Mat(full_mat) => {
                let ctx2 = ctx.clone();
                Box::new(full_mat.iter().flat_map(move |cl| {
                    let mut ctx = ctx2.clone();
                    ctx.push(Ctx {
                        full_cla: self,
                        beta_cla: cp.rest.iter().copied().collect(),
                        full_mat,
                    });
                    cl.pre_cps(ctx)
                })) as Box<dyn Iterator<Item = _>>
            }
        })
    }
}
