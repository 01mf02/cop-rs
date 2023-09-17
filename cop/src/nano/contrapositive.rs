use super::clause::VClause;
use super::matrix;
use crate::{Clause, Lit, LitMat, Matrix};
use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Nonclausal contrapositive for a literal `lit` with some inferred information about it.
#[derive(Debug)]
pub struct PreCp<'a, L, V> {
    /// the actual contrapositive
    pub contra: crate::clause::Contrapositive<&'a L, &'a LitMat<L, matrix::Matrix<L, V>>>,
    /// all clauses and matrices originally containing `lit`, largest first
    pub ctx: Vec<Ctx<'a, L, V>>,
    /// groundness of beta_cla \cup args
    pub ground: bool,
    /// maximal variable of `ctx[0].full_cla` (the largest clause containing `lit`) or
    /// (if ctx empty) beta_cla \cup args
    pub offset: Option<V>,
}

pub struct Iter<'a, L, V> {
    current: <&'a Clause<&'a LitMat<L, matrix::Matrix<L, V>>> as IntoIterator>::IntoIter,
    minimal: <&'a Clause<&'a LitMat<L, matrix::Matrix<L, V>>> as IntoIterator>::IntoIter,
    state: IterState<core::slice::Iter<'a, Ctx<'a, L, V>>>,
    ctx: &'a [Ctx<'a, L, V>],
}

#[derive(Clone)]
enum IterState<I> {
    Inward(I),
    Outward(core::iter::Rev<I>),
}

impl<'a, L, V> Clone for Iter<'a, L, V> {
    fn clone(&self) -> Self {
        Self {
            current: self.current.clone(),
            minimal: self.minimal.clone(),
            state: self.state.clone(),
            ctx: self.ctx,
        }
    }
}

impl<'a, L, V> IntoIterator for &'a PreCp<'a, L, V> {
    type IntoIter = Iter<'a, L, V>;
    type Item = &'a LitMat<L, matrix::Matrix<L, V>>;
    fn into_iter(self) -> Self::IntoIter {
        let minimal = self.contra.rest.iter();
        let (state, current) = if let Some(c) = self.ctx.first() {
            (IterState::Inward(self.ctx.iter()), c.beta_cll.iter())
        } else {
            (IterState::Outward(self.ctx.iter().rev()), minimal.clone())
        };
        Iter {
            state,
            minimal,
            current,
            ctx: &self.ctx,
        }
    }
}

impl<'a, L, V> Iterator for Iter<'a, L, V> {
    type Item = &'a LitMat<L, matrix::Matrix<L, V>>;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(x) = self.current.next() {
                return Some(x);
            } else {
                match &mut self.state {
                    IterState::Inward(ctx) => {
                        if let Some(c) = ctx.next() {
                            self.current = c.beta_cll.iter();
                        } else {
                            self.current = self.minimal.clone();
                            self.state = IterState::Outward(self.ctx.iter().rev())
                        }
                    }
                    IterState::Outward(ctx) => {
                        if let Some(c) = ctx.next() {
                            self.current = c.beta_clr.iter();
                        } else {
                            return None;
                        }
                    }
                }
            }
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

/// Saves information about any clause in which some `lit` occurs.
#[derive(Debug)]
pub struct Ctx<'a, L, V> {
    /// equivalent to `beta_cll + full_mat + beta_clr`
    full_cla: &'a VClause<L, V>,
    /// everything to the left of `full_mat`
    beta_cll: Clause<&'a LitMat<L, matrix::Matrix<L, V>>>,
    /// everything to the right of `full_mat`
    beta_clr: Clause<&'a LitMat<L, matrix::Matrix<L, V>>>,
    /// contains the literal `lit` (at any depth)
    full_mat: &'a Matrix<VClause<L, V>>,
}

impl<'a, L, V> Ctx<'a, L, V> {
    pub fn move_beta_left(&mut self) {
        self.beta_cll.append(&mut self.beta_clr);
    }

    pub fn move_beta_right(&mut self) {
        self.move_beta_left();
        core::mem::swap(&mut self.beta_cll, &mut self.beta_clr);
    }
}

impl<'a, L: Display, V: Display> Display for Ctx<'a, L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ∨ {}", self.beta_cll, self.beta_clr)
    }
}

impl<'a, L, V> Clone for Ctx<'a, L, V> {
    fn clone(&self) -> Self {
        Self {
            full_cla: self.full_cla,
            full_mat: self.full_mat,
            beta_cll: self.beta_cll.iter().cloned().collect(),
            beta_clr: self.beta_clr.iter().cloned().collect(),
        }
    }
}

impl<P, C, V: Clone + Ord> matrix::Matrix<Lit<P, C, V>, V> {
    pub fn pre_cps(&self) -> impl Iterator<Item = PreCp<Lit<P, C, V>, V>> {
        self.into_iter().flat_map(|cl| {
            let offset = cl.bound_vars().max();
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
    /// Yield all pre-contrapositives for a given clause in a given context.
    fn pre_cps<'a>(&'a self, ctx: Vec<Ctx<'a, L, V>>) -> impl Iterator<Item = PreCp<'a, L, V>> {
        use alloc::boxed::Box;
        self.1.contrapositives().flat_map(move |cp| match cp.lit {
            LitMat::Lit(lit) => Box::new(core::iter::once(PreCp {
                contra: crate::clause::Contrapositive {
                    lit,
                    pos: cp.pos,
                    rest: cp.rest,
                },
                ctx: ctx.clone(),
                // these values are just placeholders that are overwritten later
                ground: true,
                offset: None,
            })),
            LitMat::Mat(full_mat) => {
                let ctx = ctx.clone();
                Box::new(full_mat.iter().flat_map(move |cl| {
                    let mut ctx = ctx.clone();
                    ctx.push(Ctx {
                        full_cla: self,
                        beta_cll: cp.rest.iter().copied().take(cp.pos).collect(),
                        beta_clr: cp.rest.iter().copied().skip(cp.pos).collect(),
                        full_mat,
                    });
                    cl.pre_cps(ctx)
                })) as Box<dyn Iterator<Item = _>>
            }
        })
    }
}
