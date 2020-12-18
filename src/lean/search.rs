use super::{Db, Proof};
use crate::lean::clause;
use crate::lean::database::Contrapositive;
use crate::offset::{OLit, Offset, Sub};
use crate::{BackTrackStack, Lit};
use core::fmt::Display;
use core::hash::Hash;
use core::ops::Neg;
use log::debug;

pub type OClause<'t, P, C> = clause::OClause<'t, Lit<P, C, usize>>;
pub type Contras<'t, P, C> = &'t [Contrapositive<P, C, usize>];

type Index = usize;

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
    cl_skip: Index,
    path: Vec<OLit<'t, P, C>>,
    lemmas: Vec<OLit<'t, P, C>>,
}

struct TaskPtr<'t, P, C> {
    cl: OClause<'t, P, C>,
    cl_skip: Index,
    path_len: usize,
    lemmas_len: usize,
}

impl<'t, P, C> Clone for TaskPtr<'t, P, C> {
    fn clone(&self) -> Self {
        Self {
            cl: self.cl,
            cl_skip: self.cl_skip,
            path_len: self.path_len,
            lemmas_len: self.lemmas_len,
        }
    }
}

struct SubPtr {
    dom_len: usize,
    dom_max: usize,
}

struct Alternative<'t, P, C> {
    task: TaskPtr<'t, P, C>,
    promises_len: usize,
    proof_len: usize,
    sub: SubPtr,
}

pub struct Opt {
    pub lim: usize,
    pub cut: bool,
}

pub struct Search<'t, P, C> {
    task: Task<'t, P, C>,
    alternatives: Vec<(Alternative<'t, P, C>, Action<'t, P, C>)>,
    promises: BackTrackStack<(TaskPtr<'t, P, C>, usize)>,
    proof: Vec<Action<'t, P, C>>,
    pub sub: Sub<'t, C>,
    db: &'t Db<P, C, usize>,
    inferences: usize,
    literals: usize,
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

impl<'t, P, C> Rewind<TaskPtr<'t, P, C>> for Task<'t, P, C> {
    fn rewind(&mut self, state: TaskPtr<'t, P, C>) {
        self.cl = state.cl;
        self.cl_skip = state.cl_skip;
        self.path.truncate(state.path_len);
        self.lemmas.truncate(state.lemmas_len);
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

impl<'t, C> Rewind<&SubPtr> for Sub<'t, C> {
    fn rewind(&mut self, state: &SubPtr) {
        self.set_dom_len(state.dom_len);
        self.set_dom_max(state.dom_max);
    }
}

impl<'t, P, C> From<&Search<'t, P, C>> for Alternative<'t, P, C> {
    fn from(st: &Search<'t, P, C>) -> Self {
        Self {
            task: TaskPtr::from(&st.task),
            promises_len: st.promises.len(),
            proof_len: st.proof.len(),
            sub: SubPtr::from(&st.sub),
        }
    }
}

impl<'t, P, C> Rewind<Alternative<'t, P, C>> for Search<'t, P, C> {
    fn rewind(&mut self, alt: Alternative<'t, P, C>) {
        self.task.rewind(alt.task);
        assert!(self.promises.len() >= alt.promises_len);
        self.promises.truncate(alt.promises_len);
        assert!(self.proof.len() >= alt.proof_len);
        self.proof.truncate(alt.proof_len);
        self.sub.rewind(&alt.sub);
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

impl<'t, P, C> Search<'t, P, C> {
    pub fn new(task: Task<'t, P, C>, db: &'t Db<P, C, usize>, opt: Opt) -> Self {
        Self {
            db,
            task,
            sub: Sub::default(),
            alternatives: Vec::new(),
            promises: BackTrackStack::new(),
            proof: Vec::new(),
            inferences: 0,
            literals: 0,
            opt,
        }
    }
}

#[derive(Clone)]
pub enum Action<'t, P, C> {
    Prove,
    Reduce(OLit<'t, P, C>, Index),
    Extend(OLit<'t, P, C>, Contras<'t, P, C>, Index),
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
                Action::Prove => match self.task.lits().next() {
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
        debug!("lemmas: {}", self.task.lemmas.len());
        debug!("path: {}", self.task.path.len());
        self.literals += 1;
        if self.task.regularity(&self.sub) {
            debug!("regularity");
            self.try_alternative()
        } else if self.task.lem_chk(&self.sub, lit) {
            debug!("lemma");
            self.proof.push(Action::Prove);
            // do not add lit to lemmas, unlike original leanCoP
            // furthermore, do not try red/ext steps if we found a lemma,
            // because it does not add anything to substitution
            // note that Jens said that this might sometimes be counterproductive,
            // because adding to the substitution is also beneficial to cut down search space
            self.task.cl_skip += 1;
            Ok(Action::Prove)
        } else {
            Ok(Action::Reduce(lit, 0))
        }
    }

    fn red(&mut self, lit: OLit<'t, P, C>, skip: usize) -> State<'t, P, C> {
        debug!("reduce: {}", lit);
        let alternative = Alternative::from(&*self);
        for (pidx, pat) in self.task.path.iter().rev().enumerate().skip(skip) {
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
                self.task.lemmas.push(lit);
                self.task.cl_skip += 1;
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
        let alternative = Alternative::from(&*self);
        let sub = SubPtr::from(&self.sub);
        for (eidx, entry) in cs.iter().enumerate().skip(skip) {
            debug!(
                "try extend {}{} (lit = {}, |path| = {})",
                lit.head(),
                entry,
                lit,
                self.task.path.len()
            );
            if self.task.path.len() >= self.opt.lim && entry.vars.is_some() {
                debug!("path limit reached");
                continue;
            };
            let eargs = Offset::new(sub.dom_max, &entry.args);
            if let Some(vars) = entry.vars {
                // we have to add 1 here because the lowest variable is 0
                self.sub.set_dom_max(sub.dom_max + vars + 1)
            };
            debug!("unify {} ~? {}, sub = {}", eargs, lit.args(), self.sub);
            if eargs.unify(&mut self.sub, lit.args()) {
                debug!("unify succeeded with {}, sub = {}", entry.rest, self.sub);
                self.inferences += 1;

                // promise to fulfill the current task
                // (if the promise is kept and cut is enabled,
                // then all alternatives that came after will be discarded)
                // TODO: use alternative.task here
                let promise = TaskPtr::from(&self.task);
                self.promises.store();
                self.promises.push((promise, self.alternatives.len()));

                self.proof.push(Action::Extend(lit, cs, eidx));
                let action = Action::Extend(lit, cs, eidx + 1);
                // register an alternative (that will be discarded
                // if the above promise is kept and cut is enabled)
                self.alternatives.push((alternative, action));

                self.task.path.push(lit);
                self.task.cl = Offset::new(sub.dom_max, &entry.rest);
                self.task.cl_skip = 0;
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
        self.promises.pop().ok_or(true).map(|(promise, alt_len)| {
            self.task.rewind(promise);
            self.task.advance();
            if self.opt.cut {
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
