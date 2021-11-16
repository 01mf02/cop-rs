use super::{Db, PreCp};
use crate::lean::{context, cuts, Cuts};
use crate::offset::{OLit, Offset, Sub};
use crate::subst::Ptr as SubPtr;
use crate::{Lit, LitMat, PutRewind, Rewind};
use alloc::vec::Vec;
use core::{fmt::Display, hash::Hash, ops::Neg};
use log::debug;

pub struct Search<'t, P, C> {
    task: Task<'t, P, C>,
    promises: Vec<Promise<Task<'t, P, C>>>,
    alternatives: Vec<(Alternative<'t, P, C>, Action<'t, P, C>)>,
    pub sub: Sub<'t, C>,
    ctx: Context<'t, P, C>,
    proof: Vec<Action<'t, P, C>>,
    db: &'t Db<'t, P, C, usize>,
    opt: Opt,
    inferences: usize,
}

type OClauseIter<'t, L> = <crate::clause::OClause<'t, L> as IntoIterator>::IntoIter;

type OPreCp<'t, L> = Offset<&'t PreCp<'t, L, usize>>;
type OPreCpIter<'t, L> = <OPreCp<'t, L> as IntoIterator>::IntoIter;

pub enum Task<'t, P, C> {
    Dec(OClauseIter<'t, LiMa<P, C, usize>>),
    Ext(OPreCpIter<'t, Lit<P, C, usize>>),
}

impl<'t, P, C> Clone for Task<'t, P, C> {
    fn clone(&self) -> Self {
        match self {
            Self::Dec(d) => Self::Dec(d.clone()),
            Self::Ext(e) => Self::Ext(e.clone()),
        }
    }
}

impl<'t, P, C> Iterator for Task<'t, P, C> {
    type Item = OLitMat<'t, P, C>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Dec(d) => d.next(),
            Self::Ext(e) => e.next(),
        }
    }
}

pub type Context<'t, P, C> = crate::lean::Context<Vec<OLit<'t, P, C>>>;

pub type OMat<'t, P, C> = Offset<&'t super::Matrix<Lit<P, C, usize>, usize>>;

#[derive(Clone, Debug)]
pub enum Action<'t, P, C> {
    Prove,
    Reduce(OLit<'t, P, C>, Index),
    Extend(OLit<'t, P, C>, Contras<'t, P, C>, Index),
    Decompose(OMat<'t, P, C>, Index),
}

type Index = usize;
type Contras<'t, P, C> = &'t [PreCp<'t, Lit<P, C, usize>, usize>];

struct Alternative<'t, P, C> {
    task: Task<'t, P, C>,
    ctx: PutRewind<Context<'t, P, C>, context::Ptr>,
    promises: PutRewind<Vec<Promise<Task<'t, P, C>>>, usize>,
    sub: SubPtr,
    proof_len: usize,
}

#[derive(Clone)]
struct Promise<T> {
    task: T,
    ctx_ptr: context::Ptr,
    alt_len: usize,
}

pub struct Opt {
    pub lim: usize,
    pub cuts: Cuts,
}

type LiMa<P, C, V> = LitMat<Lit<P, C, V>, super::Matrix<Lit<P, C, V>, V>>;
pub type OLitMat<'t, P, C> = Offset<&'t LiMa<P, C, usize>>;

use crate::Clause;

impl<'t, P, C> Search<'t, P, C> {
    pub fn new(cl: &'t Clause<LiMa<P, C, usize>>, db: &'t Db<P, C, usize>, opt: Opt) -> Self {
        let mut sub = Sub::default();
        sub.set_dom_max(cl.bound_vars().max().map(|v| v + 1).unwrap_or(0));
        Self {
            task: Task::Dec(Offset::new(0, cl).into_iter()),
            promises: Vec::new(),
            alternatives: Vec::new(),
            sub,
            ctx: Context::default(),
            proof: Vec::new(),
            db,
            opt,
            inferences: 0,
        }
    }

    pub fn inferences(&self) -> usize {
        self.inferences
    }
}

type State<'t, P, C> = Result<Action<'t, P, C>, bool>;

impl<'t, P, C> Search<'t, P, C>
where
    P: Clone + Display + Eq + Hash + Neg<Output = P>,
    C: Clone + Display + Eq,
{
    pub fn prove(&mut self) -> Option<&Vec<Action<'t, P, C>>> {
        let mut action: Action<'t, P, C> = Action::Prove;
        loop {
            let result = match action {
                Action::Prove => self.chk(),
                Action::Reduce(lit, skip) => self.red(lit, skip),
                Action::Extend(lit, contras, skip) => self.ext(lit, contras, skip),
                Action::Decompose(mat, skip) => self.decompose(mat, skip),
            };
            match result {
                Ok(next) => action = next,
                Err(true) => return Some(&self.proof),
                Err(false) => return None,
            }
        }
    }

    /// Regularity and lemma check.
    fn chk(&mut self) -> State<'t, P, C> {
        debug!("regularity check");
        debug!("path: {}", self.ctx.path.len());

        let mut lits = self.task.clone().filter_map(|olm| olm.transpose().lit());
        if lits.any(|cl| self.ctx.path.iter().any(|pl| pl.eq_mod(&self.sub, &cl))) {
            debug!("regularity");
            self.try_alternative()
        } else {
            match self.task.clone().next().map(|lm| lm.transpose()) {
                Some(LitMat::Lit(lit)) => Ok(self.lem(lit)),
                Some(LitMat::Mat(mat)) => self.decompose(mat, 0),
                None => self.fulfill_promise(),
            }
        }
    }

    /// Lemma check.
    fn lem(&mut self, lit: OLit<'t, P, C>) -> Action<'t, P, C> {
        debug!("lemma check: {}", lit);
        debug!("lemmas: {}", self.ctx.lemmas.len());

        let mut lemmas = self.ctx.lemmas.iter();
        if lemmas.any(|lem| lem.eq_mod(&self.sub, &lit)) {
            debug!("lemma");
            self.proof.push(Action::Prove);
            // do not add lit to lemmas, unlike original leanCoP
            // furthermore, do not try red/ext steps if we found a lemma,
            // because it does not add anything to substitution
            // note that Jens said that this might sometimes be counterproductive,
            // because adding to the substitution is also beneficial to cut down search space
            self.task.next();
            Action::Prove
        } else {
            Action::Reduce(lit, 0)
        }
    }

    fn red(&mut self, lit: OLit<'t, P, C>, skip: usize) -> State<'t, P, C> {
        debug!("reduce: {}", lit);
        let alternative = Alternative::from(&*self);
        for (pidx, pat) in self.ctx.path.iter().rev().enumerate().skip(skip) {
            debug!("try reduce: {}", pat);
            let sub_dom_len = self.sub.get_dom_len();
            if pat.head() != &-lit.head().clone() {
                continue;
            }
            if pat.args().unify(&mut self.sub, lit.args()) {
                debug!("reduce succeeded");
                self.proof.push(Action::Reduce(lit, pidx));
                if !self.opt.cuts.reduction {
                    let action = Action::Reduce(lit, pidx + 1);
                    self.alternatives.push((alternative, action));
                }
                self.ctx.lemmas.push(lit);
                self.task.next();
                return Ok(Action::Prove);
            } else {
                self.sub.set_dom_len(sub_dom_len)
            }
        }

        self.ext0(lit)
    }

    fn ext0(&mut self, lit: OLit<'t, P, C>) -> State<'t, P, C> {
        debug!("extend: {}", lit);
        let neg = -lit.head().clone();
        match self.db.get(&neg) {
            Some(entries) => self.ext(lit, entries, 0),
            None => self.try_alternative(),
        }
    }

    fn ext(&mut self, lit: OLit<'t, P, C>, cs: Contras<'t, P, C>, skip: usize) -> State<'t, P, C> {
        let alt = Alternative::from(&*self);
        let prm = Promise::from(&*self);
        let sub = SubPtr::from(&self.sub);
        for (eidx, entry) in cs.iter().enumerate().skip(skip) {
            debug!(
                "try extend {}{} (lit = {}, |path| = {})",
                lit.head(),
                entry,
                lit,
                self.ctx.path.len()
            );
            if self.ctx.path.len() >= self.opt.lim && !entry.ground {
                debug!("path limit reached");
                continue;
            };
            let eargs = Offset::new(sub.dom_max(), entry.contra.lit.args());
            if let Some(vars) = entry.offset {
                // we have to add 1 here because the lowest variable is 0
                self.sub.set_dom_max(sub.dom_max() + vars + 1)
            };
            debug!("unify {} ~? {}, sub = {}", eargs, lit.args(), self.sub);
            if eargs.unify(&mut self.sub, lit.args()) {
                debug!("unify succeeded with {}, sub = {}", entry.contra, self.sub);
                self.inferences += 1;

                // promise to fulfill the current task
                // (if the promise is kept and cut is enabled,
                // then all alternatives that came after will be discarded)
                self.promises.push(prm);

                self.proof.push(Action::Extend(lit, cs, eidx));
                let action = Action::Extend(lit, cs, eidx + 1);
                // register an alternative (that will be discarded
                // if the above promise is kept and cut is enabled)
                self.alternatives.push((alt, action));

                self.task = Task::Ext(Offset::new(sub.dom_max(), entry).into_iter());
                self.ctx.path.push(lit);
                return Ok(Action::Prove);
            } else {
                debug!("unify failed");
                self.sub.rewind(&sub)
            }
        }

        self.try_alternative()
    }

    fn decompose(&mut self, mat: OMat<'t, P, C>, skip: usize) -> State<'t, P, C> {
        if let Some(cl) = mat.into_iter().nth(skip) {
            let alt = Alternative::from(&*self);
            let prm = Promise::from(&*self);

            self.promises.push(prm);

            self.proof.push(Action::Decompose(mat, skip));
            let action = Action::Decompose(mat, skip + 1);
            self.alternatives.push((alt, action));

            self.task = Task::Dec(cl.map(|vcl| &vcl.1).into_iter());
            Ok(Action::Prove)
        } else {
            self.try_alternative()
        }
    }

    fn fulfill_promise(&mut self) -> State<'t, P, C> {
        debug!("fulfill promise ({} left)", self.promises.len());
        let prm = self.promises.pop().ok_or(true)?;
        self.task = prm.task;
        self.ctx.rewind(prm.ctx_ptr);
        let prev = self.task.next().map(|lm| lm.transpose()).unwrap();
        let cut = match prev {
            LitMat::Lit(prev) => {
                self.ctx.lemmas.push(prev);
                self.opt.cuts.extension
            }
            LitMat::Mat(_) => self.opt.cuts.decomposition,
        };
        if let Some(cut) = cut {
            use cuts::Cut::*;
            let alt_len = match cut {
                Exclusive => prm.alt_len + 1,
                Inclusive => prm.alt_len,
            };
            debug!("cut {} alternatives", self.alternatives.len() - alt_len);
            assert!(alt_len <= self.alternatives.len());
            self.alternatives.truncate(alt_len);
        }
        Ok(Action::Prove)
    }

    fn try_alternative(&mut self) -> State<'t, P, C> {
        debug!("try alternative ({} left)", self.alternatives.len());
        self.alternatives.pop().ok_or(false).map(|(alt, action)| {
            self.rewind(alt);
            action
        })
    }
}

impl<'t, P, C> From<&Search<'t, P, C>> for Alternative<'t, P, C> {
    fn from(st: &Search<'t, P, C>) -> Self {
        Self {
            task: st.task.clone(),
            ctx: if st.opt.cuts.backtracking_may_grow() {
                // when we do *not* cut on extension steps, then we may need to
                // backtrack to contexts that are larger than the current context,
                // so we save the whole context here
                PutRewind::Put(st.ctx.clone())
            } else {
                // when we cut on extension steps, then we always
                // backtrack to contexts that are prefixes of the current context,
                // so storing just a pointer to the context suffices
                PutRewind::Rewind(context::Ptr::from(&st.ctx))
            },
            promises: if st.opt.cuts.backtracking_may_grow() {
                PutRewind::Put(st.promises.clone())
            } else {
                PutRewind::Rewind(st.promises.len())
            },
            sub: SubPtr::from(&st.sub),
            proof_len: st.proof.len(),
        }
    }
}

impl<'t, P, C> From<&Search<'t, P, C>> for Promise<Task<'t, P, C>> {
    fn from(st: &Search<'t, P, C>) -> Self {
        Self {
            task: st.task.clone(),
            ctx_ptr: context::Ptr::from(&st.ctx),
            alt_len: st.alternatives.len(),
        }
    }
}

impl<'t, P, C> Rewind<Alternative<'t, P, C>> for Search<'t, P, C> {
    fn rewind(&mut self, alt: Alternative<'t, P, C>) {
        self.task = alt.task;

        self.ctx.rewind(alt.ctx);
        self.promises.rewind(alt.promises);
        self.sub.rewind(&alt.sub);
        self.proof.rewind(alt.proof_len);
    }
}
