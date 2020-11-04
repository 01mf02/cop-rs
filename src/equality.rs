use crate::term::{Args, Arity};
use crate::{Form, Term};
use std::collections::HashMap;

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
    pub fn eq_axioms(preds: HashMap<&P, Arity>, consts: HashMap<&C, Arity>) -> Self {
        let consts = consts.into_iter().filter_map(|(p, arity)| {
            let app = |args| Term::C(p.clone(), args);
            Self::eq_subst(arity, |eqs, al, ar| {
                Form::imp(eqs, Form::EqTm(app(al), app(ar)))
            })
        });
        let preds = preds.into_iter().filter_map(|(p, arity)| {
            let app = |args| Form::Atom(p.clone(), args);
            Self::eq_subst(arity, |eqs, al, ar| Form::imp(eqs & app(al), app(ar)))
        });
        let init = Self::eq_refl() & (Self::eq_sym() & Self::eq_trans());
        preds.chain(consts).fold(init, |acc, fm| fm & acc)
    }
}
