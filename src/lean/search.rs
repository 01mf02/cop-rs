use super::Db;
use crate::lean::Clause;
use crate::offset::{OLit, Offset, Sub};
use crate::Lit;
use core::fmt::Display;
use core::hash::Hash;
use core::ops::Neg;
use log::debug;

pub type OClause<'t, P, C> = Offset<&'t Clause<Lit<P, C, usize>>>;

struct Proof {}

/// Restore the state of mutable data structures.
///
/// In the presence of backtracking, mutable data structures
/// (such as the substitution) often need to be reset to an earlier state.
/// Such data structures should implement `Rewind<T>` if
/// `T` is a cheap and small characterisation of their state.
trait Rewind<T> {
    fn rewind(&mut self, state: T);
}

pub struct Task<'t, P, C> {
    cl: OClause<'t, P, C>,
    cl_skip: usize,
    path: Vec<OLit<'t, P, C>>,
    lemmas: Vec<OLit<'t, P, C>>,
}

struct TaskPtr<'t, P, C> {
    cl: OClause<'t, P, C>,
    cl_skip: usize,
    path_len: usize,
    lemmas_len: usize,
}

struct SubPtr {
    dom_len: usize,
    dom_max: usize,
}

struct Alternative<'t, P, C> {
    task: TaskPtr<'t, P, C>,
    promises_len: usize,
    sub: SubPtr,
    proofs_len: usize,
}

struct Promise<'t, P, C> {
    task: TaskPtr<'t, P, C>,
    alternatives_len: usize,
}

pub struct Opt {
    pub lim: usize,
    pub cut: bool,
}

pub struct Search<'t, P, C> {
    task: Task<'t, P, C>,
    alternatives: Vec<(Alternative<'t, P, C>, State<'t, P, C>)>,
    promises: Vec<Promise<'t, P, C>>,
    proofs: Vec<Proof>,
    sub: Sub<'t, C>,
    db: &'t Db<P, C, usize>,
    inferences: usize,
    opt: Opt,
}

impl<'t, P, C> From<&Task<'t, P, C>> for TaskPtr<'t, P, C> {
    fn from(task: &Task<'t, P, C>) -> Self {
        Self {
            cl: task.cl,
            cl_skip: task.cl_skip,
            path_len: task.path.len(),
            lemmas_len: task.lemmas.len(),
        }
    }
}

impl<'t, C> From<&Sub<'t, C>> for SubPtr {
    fn from(sub: &Sub<'t, C>) -> Self {
        Self {
            dom_len: sub.get_dom_len(),
            dom_max: sub.get_dom_max(),
        }
    }
}

impl<'t, C> Rewind<SubPtr> for Sub<'t, C> {
    fn rewind(&mut self, state: SubPtr) {
        self.set_dom_len(state.dom_len);
        self.set_dom_max(state.dom_max);
    }
}

impl<'t, P, C> Rewind<TaskPtr<'t, P, C>> for Task<'t, P, C> {
    fn rewind(&mut self, state: TaskPtr<'t, P, C>) {
        self.cl = state.cl;
        self.cl_skip = state.cl_skip;
        self.path.truncate(state.path_len);
        self.lemmas.truncate(state.lemmas_len);
    }
}

impl<'t, P, C> Rewind<Promise<'t, P, C>> for Search<'t, P, C> {
    fn rewind(&mut self, prm: Promise<'t, P, C>) {
        self.task.rewind(prm.task);
        // TODO: is this really the right place for cut?
        if self.opt.cut {
            self.alternatives.truncate(prm.alternatives_len)
        }
    }
}

impl<'t, P, C> Rewind<Alternative<'t, P, C>> for Search<'t, P, C> {
    fn rewind(&mut self, alt: Alternative<'t, P, C>) {
        self.task.rewind(alt.task);
        self.promises.truncate(alt.promises_len);
        self.proofs.truncate(alt.proofs_len);
        self.sub.rewind(alt.sub);
    }
}

impl<'t, P, C> Task<'t, P, C> {
    pub fn new(cl: OClause<'t, P, C>) -> Self {
        Self {
            cl,
            cl_skip: 0,
            path: Vec::new(),
            lemmas: Vec::new(),
        }
    }

    fn lits(&self) -> impl Iterator<Item = OLit<'t, P, C>> {
        self.cl.into_iter().skip(self.cl_skip)
    }

    fn advance(&mut self) {
        if let Some(prev) = self.lits().next() {
            self.lemmas.push(prev);
            self.cl_skip += 1;
        }
    }
}

impl<'t, P: Eq, C: Eq> Task<'t, P, C> {
    fn regularity(&self, sub: &Sub<'t, C>) -> bool {
        self.lits()
            .any(|cl| self.path.iter().any(|pl| pl.eq_mod(sub, &cl)))
    }

    fn lem_chk(&self, sub: &Sub<'t, C>, lit: OLit<'t, P, C>) -> bool {
        self.lemmas.iter().any(|lem| lem.eq_mod(sub, &lit))
    }
}

impl<'t, P, C> From<&Search<'t, P, C>> for Promise<'t, P, C> {
    fn from(st: &Search<'t, P, C>) -> Self {
        Self {
            task: TaskPtr::from(&st.task),
            alternatives_len: st.alternatives.len(),
        }
    }
}

impl<'t, P, C> From<&Search<'t, P, C>> for Alternative<'t, P, C> {
    fn from(st: &Search<'t, P, C>) -> Self {
        Self {
            task: TaskPtr::from(&st.task),
            sub: SubPtr::from(&st.sub),
            promises_len: st.promises.len(),
            proofs_len: st.proofs.len(),
        }
    }
}

impl<'t, P, C> Search<'t, P, C> {
    pub fn new(task: Task<'t, P, C>, db: &'t Db<P, C, usize>, opt: Opt) -> Self {
        Self {
            db,
            task,
            sub: Default::default(),
            alternatives: Vec::new(),
            promises: Vec::new(),
            proofs: Vec::new(),
            inferences: 0,
            opt,
        }
    }
}

enum State<'t, P, C> {
    Prove,
    Reduce(OLit<'t, P, C>, usize),
    Extend(OLit<'t, P, C>, usize),
    Done(bool),
}

impl<'t, P, C> Search<'t, P, C>
where
    P: Clone + Display + Eq + Hash + Neg<Output = P>,
    C: Clone + Display + Eq,
{
    pub fn prove(&mut self) -> bool {
        let mut state: State<'t, P, C> = State::Prove;
        loop {
            state = match state {
                State::Prove => match self.task.lits().next() {
                    Some(lit) => self.checks(lit),
                    None => self.fulfill_promise(),
                },
                State::Reduce(lit, skip) => self.reduce(lit, skip),
                State::Extend(lit, skip) => self.extend(lit, skip),
                State::Done(status) => return status,
            }
        }
    }

    fn checks(&mut self, lit: OLit<'t, P, C>) -> State<'t, P, C> {
        debug!("checks: {}", lit);
        if self.task.regularity(&self.sub) {
            debug!("regularity");
            self.try_alternative()
        } else if self.task.lem_chk(&self.sub, lit) {
            debug!("lemma");
            // do not add lit to lemmas, unlike original leanCoP
            self.task.cl_skip += 1;
            State::Prove
        } else {
            State::Reduce(lit, 0)
        }
    }

    fn reduce(&mut self, lit: OLit<'t, P, C>, skip: usize) -> State<'t, P, C> {
        debug!("reduce: {}", lit);
        for (pidx, pat) in self.task.path.iter().rev().enumerate().skip(skip) {
            debug!("try reduce: {}", pat);
            let sub_dom_len = self.sub.get_dom_len();
            if pat.head() != &-lit.head().clone() {
                continue;
            }
            if pat.args().unify(&mut self.sub, lit.args()) {
                debug!("reduce succeeded");
                let alternative = Alternative::from(&*self);
                let ckpt = State::Reduce(lit, pidx + 1);
                self.alternatives.push((alternative, ckpt));
                self.task.cl_skip += 1;
                return State::Prove;
            } else {
                self.sub.set_dom_len(sub_dom_len)
            }
        }
        State::Extend(lit, 0)
    }

    fn extend(&mut self, lit: OLit<'t, P, C>, skip: usize) -> State<'t, P, C> {
        debug!("extend: {}", lit);
        let neg = -lit.head().clone();
        debug!("neg: {}", neg);
        if let Some(entries) = self.db.get(&neg) {
            for (eidx, entry) in entries.iter().enumerate().skip(skip) {
                debug!("try extend (path len = {})", self.task.path.len());
                if self.task.path.len() >= self.opt.lim && entry.vars.is_some() {
                    continue;
                };
                let sub_dom_len = self.sub.get_dom_len();
                let sub_dom_max = self.sub.get_dom_max();
                let eargs = Offset::new(sub_dom_max, &entry.args);
                if let Some(vars) = entry.vars {
                    self.sub.set_dom_max(sub_dom_max + vars + 1)
                };
                debug!("extend {} ~? {}, sub = {}", eargs, lit.args(), self.sub);
                if eargs.unify(&mut self.sub, lit.args()) {
                    debug!("unify succeeded");
                    self.inferences += 1;
                    let alternative = Alternative::from(&*self);
                    let ckpt = State::Extend(lit, eidx + 1);
                    self.alternatives.push((alternative, ckpt));
                    self.promises.push(Promise::from(&*self));
                    self.task.path.push(lit);
                    self.task.cl = Offset::new(sub_dom_max, &entry.rest);
                    self.task.cl_skip = 0;
                    return State::Prove;
                } else {
                    debug!("unify failed");
                    self.sub.set_dom_max(sub_dom_max);
                    self.sub.set_dom_len(sub_dom_len);
                }
            }
        }
        self.try_alternative()
    }

    fn fulfill_promise(&mut self) -> State<'t, P, C> {
        match self.promises.pop() {
            Some(promise) => {
                self.rewind(promise);
                self.task.advance();
                State::Prove
            }
            None => State::Done(true),
        }
    }

    fn try_alternative(&mut self) -> State<'t, P, C> {
        match self.alternatives.pop() {
            Some((alternative, ckpt)) => {
                self.rewind(alternative);
                ckpt
            }
            None => State::Done(false),
        }
    }
}
