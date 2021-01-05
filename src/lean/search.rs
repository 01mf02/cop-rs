use super::context;
use super::Contrapositive;
use super::{Db, Proof};
use crate::offset::{OLit, Offset, Sub};
use crate::subst::Ptr as SubPtr;
use crate::{BackTrackStack, Lit, Rewind, Skipper};
use core::fmt::Display;
use core::hash::Hash;
use core::ops::Neg;
use log::debug;

pub struct Search<'t, P, C> {
    task: Task<'t, P, C>,
    ctx: Context<'t, P, C>,
    pub sub: Sub<'t, C>,
    proof: Vec<Action<'t, P, C>>,
    promises: BackTrackStack<(Task<'t, P, C>, context::Ptr, usize)>,
    alternatives: Vec<(Alternative<'t, P, C>, Action<'t, P, C>)>,
    inferences: usize,
    literals: usize,
    db: &'t Db<P, C, usize>,
    opt: Opt,
}

pub type Task<'t, P, C> = Skipper<super::clause::OClause<'t, Lit<P, C, usize>>>;
pub type Context<'t, P, C> = context::Context<Vec<OLit<'t, P, C>>>;

#[derive(Clone)]
pub enum Action<'t, P, C> {
    Prove,
    Reduce(OLit<'t, P, C>, Index),
    Extend(OLit<'t, P, C>, Contras<'t, P, C>, Index),
}

type Index = usize;
type Contras<'t, P, C> = &'t [Contrapositive<P, C, usize>];

struct Alternative<'t, P, C> {
    task: Task<'t, P, C>,
    ctx: context::Ptr,
    sub: SubPtr,
    proof_len: usize,
    promises_len: usize,
}

pub struct Opt {
    pub lim: usize,
    pub cut: bool,
    pub cutalt: bool,
}

impl<'t, P, C> Search<'t, P, C> {
    pub fn new(task: Task<'t, P, C>, db: &'t Db<P, C, usize>, opt: Opt) -> Self {
        Self {
            task,
            ctx: Context::default(),
            sub: Sub::default(),
            proof: Vec::new(),
            promises: BackTrackStack::new(),
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
    pub fn prove(&mut self) -> Option<Proof<'t, P, C>> {
        let mut action: Action<'t, P, C> = Action::Prove;
        loop {
            let result = match action {
                Action::Prove => match self.task.iter().next() {
                    Some(lit) => self.chk(lit),
                    None => self.fulfill_promise(),
                },
                Action::Reduce(lit, skip) => self.red(lit, skip),
                Action::Extend(lit, contras, skip) => self.ext(lit, contras, skip),
            };
            match result {
                Ok(next) => action = next,
                Err(true) => {
                    return Some(Proof::from_iter(&mut self.proof.iter().cloned(), &mut 0))
                }
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

        let mut lits = self.task.iter();
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
                if !self.opt.cut {
                    let action = Action::Reduce(lit, pidx + 1);
                    self.alternatives.push((alternative, action));
                    // TODO: is this necessary?
                    self.promises.store();
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
                let alts = self.alternatives.len();
                self.promises.store();
                self.promises.push((alt.task, alt.ctx, alts));

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
        self.promises.pop().ok_or(true).map(|(task, ctx, alt_len)| {
            self.task = task;
            self.ctx.rewind(ctx);
            if let Some(prev) = self.task.next() {
                self.ctx.lemmas.push(prev)
            };
            if self.opt.cut {
                let alt_len = alt_len + self.opt.cutalt as usize;
                debug!("cut {} alternatives", self.alternatives.len() - alt_len);
                assert!(alt_len <= self.alternatives.len());
                self.alternatives.truncate(alt_len);
            }
            Action::Prove
        })
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
            task: st.task,
            ctx: context::Ptr::from(&st.ctx),
            sub: SubPtr::from(&st.sub),
            proof_len: st.proof.len(),
            promises_len: st.promises.len(),
        }
    }
}

impl<'t, P, C> Rewind<Alternative<'t, P, C>> for Search<'t, P, C> {
    fn rewind(&mut self, alt: Alternative<'t, P, C>) {
        self.task = alt.task;
        self.ctx.rewind(alt.ctx);
        self.sub.rewind(&alt.sub);
        assert!(self.proof.len() >= alt.proof_len);
        self.proof.truncate(alt.proof_len);
        assert!(self.promises.len() >= alt.promises_len);
        self.promises.truncate(alt.promises_len);
    }
}
