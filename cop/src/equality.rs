use crate::term::{Args, Arity};
use crate::{Form, Term};
use alloc::vec::Vec;

impl<P, C> Form<P, C, usize> {
    fn eq_refl() -> Self {
        let xx = Form::EqTm(Term::V(0), Term::V(0));
        Form::forall(0, xx)
    }

    fn eq_sym() -> Self {
        let vars = [0, 1].iter().rev().cloned();
        let xy = Form::EqTm(Term::V(0), Term::V(1));
        let yx = Form::EqTm(Term::V(1), Term::V(0));
        Form::foralls(vars, Form::imp(xy, yx))
    }

    fn eq_trans() -> Self {
        let vars = [0, 1, 2].iter().rev().cloned();
        let xy = Form::EqTm(Term::V(0), Term::V(1));
        let yz = Form::EqTm(Term::V(1), Term::V(2));
        let xz = Form::EqTm(Term::V(0), Term::V(2));
        Form::foralls(vars, Form::imp(xy & yz, xz))
    }

    /// Return `0 = 1 & (2 = 3 & (..))`, consisting of n conjuncts.
    ///
    /// Return `None` if `n` is 0.
    fn equalities(n: Arity) -> Option<Self> {
        let prems = (0..n).map(|v| Form::EqTm(Term::V(2 * v), Term::V(1 + 2 * v)));
        let mut prems = prems.rev();
        let last = prems.next()?;
        Some(prems.fold(last, |acc, eq| eq & acc))
    }

    fn eq_subst<F>(arity: Arity, f: F) -> Option<Self>
    where
        F: FnOnce(Self, Args<C, usize>, Args<C, usize>) -> Self,
    {
        let eqs = Self::equalities(arity)?;
        let al = (0..arity).map(|v| Term::V(2 * v)).collect();
        let ar = (0..arity).map(|v| Term::V(1 + 2 * v)).collect();
        Some(Form::foralls((0..2 * arity).rev(), f(eqs, al, ar)))
    }
}

impl<P: Clone, C: Clone> Form<P, C, usize> {
    /// Produce the substitution axiom for the given constant of arity.
    pub fn eq_constant(c: &C, arity: Arity) -> Option<Self> {
        let app = |args| Term::C(c.clone(), args);
        Self::eq_subst(arity, |eqs, al, ar| {
            Form::imp(eqs, Form::EqTm(app(al), app(ar)))
        })
    }

    /// Produce the substitution axiom for the given predicate of arity.
    pub fn eq_predicate(p: &P, arity: Arity) -> Option<Self> {
        let app = |args| Form::Atom(p.clone(), args);
        Self::eq_subst(arity, |eqs, al, ar| Form::imp(eqs & app(al), app(ar)))
    }

    /// Produce equality axioms from a sequence of predicates and constants.
    ///
    /// Assume that `c1, ..., cm` and `p1, ..., pn` are
    /// substitution axioms for the given constants and predicates.
    /// Then the final output will be
    /// `c1 & ... & cm & p1 & ... & pn & refl & sym & trans`,
    /// where the conjunction is associated to the right.
    pub fn eq_axioms(preds: Vec<(&P, Arity)>, consts: Vec<(&C, Arity)>) -> Self {
        let init = Self::eq_refl() & (Self::eq_sym() & Self::eq_trans());
        let p = preds
            .into_iter()
            .filter_map(|(p, arity)| Self::eq_predicate(p, arity));
        let c = consts
            .into_iter()
            .filter_map(|(c, arity)| Self::eq_constant(c, arity));
        init.conjoin_right(p.rev()).conjoin_right(c.rev())
    }
}
