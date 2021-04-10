use super::context;
use super::Contrapositive;
use super::{Cuts, Db, Steps};
use crate::offset::{OLit, Offset, Sub};
use crate::subst::Ptr as SubPtr;
use crate::{Lit, Rewind};
use alloc::vec::Vec;
use core::{fmt::Display, hash::Hash, ops::Neg};
use log::debug;

pub struct Search<'t, P, C> {
    task: Task<'t, P, C>,
    ctx: Context<'t, P, C>,
    promises: Vec<Promise<Task<'t, P, C>>>,
    pub sub: Sub<'t, C>,
    proof: Steps<'t, P, C>,
    alternatives: Vec<(Alternative<'t, P, C>, Action<'t, P, C>)>,
    inferences: usize,
    literals: usize,
    db: &'t Db<P, C, usize>,
    opt: Opt,
}

#[derive(Clone)]
pub struct TaskIter<C: IntoIterator>(core::iter::Skip<C::IntoIter>);

impl<C: IntoIterator> TaskIter<C> {
    pub fn new(cl: C) -> Self {
        Self(cl.into_iter().skip(0))
    }
}

impl<C: IntoIterator> Iterator for TaskIter<C> {
    type Item = <C::IntoIter as Iterator>::Item;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub type Task<'t, P, C> = TaskIter<super::clause::OClause<'t, Lit<P, C, usize>>>;

pub type Context<'t, P, C> = context::Context<Vec<OLit<'t, P, C>>>;

#[derive(Clone, Debug)]
pub enum Action<'t, P, C> {
    Prove,
    Reduce(OLit<'t, P, C>, Index),
    Extend(OLit<'t, P, C>, Contras<'t, P, C>, Index),
}

impl<'t, P, C> Action<'t, P, C> {
    pub fn max_children(&self) -> usize {
        use Action::*;
        match self {
            Prove | Reduce(_, _) => 0,
            Extend(_, cs, skip) => cs[*skip].rest.len(),
        }
    }
}

type Index = usize;
type Contras<'t, P, C> = &'t [Contrapositive<P, C, usize>];

struct Alternative<'t, P, C> {
    task: Task<'t, P, C>,
    // when we do *not* use cut, then we may need to backtrack to
    // contexts that are larger than the current context,
    // so we save the whole context here
    ctx: Option<Context<'t, P, C>>,
    // when we use cut, then we always backtrack to contexts that are
    // prefixes of the current context, so in that case,
    // storing just a pointer to the context suffices
    ctx_ptr: context::Ptr,
    promises: Option<Vec<Promise<Task<'t, P, C>>>>,
    promises_len: usize,
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

impl<'t, P, C> Search<'t, P, C> {
    pub fn new(task: Task<'t, P, C>, db: &'t Db<P, C, usize>, opt: Opt) -> Self {
        Self {
            task,
            ctx: Context::default(),
            promises: Vec::new(),
            sub: Sub::default(),
            proof: Steps::new(),
            alternatives: Vec::new(),
            inferences: 0,
            literals: 0,
            db,
            opt,
        }
    }
}

type State<'t, P, C> = Result<Action<'t, P, C>, bool>;

impl<'t, P, C> Search<'t, P, C>
where
    P: Clone + Display + Eq + Hash + Neg<Output = P>,
    C: Clone + Display + Eq,
{
    pub fn prove(&mut self) -> Option<&Steps<'t, P, C>> {
        let mut action: Action<'t, P, C> = Action::Prove;
        loop {
            let result = match action {
                Action::Prove => match self.task.clone().next() {
                    Some(lit) => self.chk(lit),
                    None => self.fulfill_promise(),
                },
                Action::Reduce(lit, skip) => self.red(lit, skip),
                Action::Extend(lit, contras, skip) => self.ext(lit, contras, skip),
            };
            match result {
                Ok(next) => action = next,
                Err(true) => return Some(&self.proof),
                Err(false) => return None,
            }
        }
    }

    pub fn inferences(&self) -> usize {
        self.inferences
    }

    fn chk(&mut self, lit: OLit<'t, P, C>) -> State<'t, P, C> {
        debug!("checks: {}", lit);
        debug!("{} {}", self.literals, lit.head());
        debug!("lemmas: {}", self.ctx.lemmas.len());
        debug!("path: {}", self.ctx.path.len());
        self.literals += 1;

        let mut lits = self.task.clone();
        let mut path = self.ctx.path.iter();
        let mut lemmas = self.ctx.lemmas.iter();
        if lits.any(|cl| path.any(|pl| pl.eq_mod(&self.sub, &cl))) {
            debug!("regularity");
            self.try_alternative()
        } else if lemmas.any(|lem| lem.eq_mod(&self.sub, &lit)) {
            debug!("lemma");
            self.proof.push(Action::Prove);
            // do not add lit to lemmas, unlike original leanCoP
            // furthermore, do not try red/ext steps if we found a lemma,
            // because it does not add anything to substitution
            // note that Jens said that this might sometimes be counterproductive,
            // because adding to the substitution is also beneficial to cut down search space
            self.task.next();
            Ok(Action::Prove)
        } else {
            Ok(Action::Reduce(lit, 0))
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
            if self.ctx.path.len() >= self.opt.lim && entry.vars.is_some() {
                debug!("path limit reached");
                continue;
            };
            let eargs = Offset::new(sub.dom_max(), &entry.args);
            if let Some(vars) = entry.vars {
                // we have to add 1 here because the lowest variable is 0
                self.sub.set_dom_max(sub.dom_max() + vars + 1)
            };
            debug!("unify {} ~? {}, sub = {}", eargs, lit.args(), self.sub);
            if eargs.unify(&mut self.sub, lit.args()) {
                debug!("unify succeeded with {}, sub = {}", entry.rest, self.sub);
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

                self.task = Task::new(Offset::new(sub.dom_max(), &entry.rest));
                self.ctx.path.push(lit);
                return Ok(Action::Prove);
            } else {
                debug!("unify failed");
                self.sub.rewind(&sub)
            }
        }

        self.try_alternative()
    }

    fn fulfill_promise(&mut self) -> State<'t, P, C> {
        debug!("fulfill promise ({} left)", self.promises.len());
        let prm = self.promises.pop().ok_or(true)?;

        self.task = prm.task;
        self.ctx.rewind(prm.ctx_ptr);
        if let Some(prev) = self.task.next() {
            self.ctx.lemmas.push(prev)
        };
        if let Some(cut) = self.opt.cuts.extension {
            use super::cuts::Cut::*;
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
            ctx: if st.opt.cuts.extension.is_none() {
                Some(st.ctx.clone())
            } else {
                None
            },
            ctx_ptr: context::Ptr::from(&st.ctx),
            promises: if st.opt.cuts.extension.is_none() {
                Some(st.promises.clone())
            } else {
                None
            },
            promises_len: st.promises.len(),
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

        if let Some(ctx) = alt.ctx {
            self.ctx = ctx;
        } else {
            self.ctx.rewind(alt.ctx_ptr);
        }

        if let Some(promises) = alt.promises {
            self.promises = promises;
        } else {
            assert!(self.promises.len() >= alt.promises_len);
            self.promises.truncate(alt.promises_len);
        }

        self.sub.rewind(&alt.sub);
        self.proof.truncate(alt.proof_len);
    }
}
