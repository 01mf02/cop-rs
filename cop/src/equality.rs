use crate::term::{Args, Arity};
use crate::{Form, Term};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use core::hash::Hash;

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
    /// Assume that `p1, ..., pn` and `c1, ..., cm` are
    /// substitution axioms for the given predicates and constants.
    /// Then the final output will be
    /// `((refl & sym & trans) & p1 & ... & pn) & c1 & ... & cm`,
    /// where the conjunction is associated to the right.
    pub fn eq_axioms(preds: Vec<(&P, Arity)>, consts: Vec<(&C, Arity)>) -> Self {
        use crate::fof::OpA;
        let init = Self::eq_refl() & (Self::eq_sym() & Self::eq_trans());
        let p = preds
            .into_iter()
            .filter_map(|(p, arity)| Self::eq_predicate(p, arity));
        let c = consts
            .into_iter()
            .filter_map(|(c, arity)| Self::eq_constant(c, arity));
        (init & Form::binas(OpA::Conj, p)) & Form::binas(OpA::Conj, c)
    }
}

impl<P: Clone + Eq + Hash, C: Clone + Eq + Hash> Form<P, C, String> {
    pub fn add_eq_axioms(self) -> Result<Self, (Vec<P>, Vec<C>)> {
        let (preds, consts) = self.predconst_unique();

        // check that all symbols occur with the same arities
        let nfpreds: Vec<_> = crate::nonfunctional(preds.clone()).cloned().collect();
        let nfconsts: Vec<_> = crate::nonfunctional(consts.clone()).cloned().collect();
        if !nfpreds.is_empty() || !nfconsts.is_empty() {
            return Err((nfpreds, nfconsts));
        }

        if self.subforms().any(|fm| matches!(fm, Form::EqTm(_, _))) {
            let axioms = Form::eq_axioms(preds, consts);
            Ok(self.add_premise(axioms.map_vars(&mut |v| v.to_string())))
        } else {
            Ok(self)
        }
    }
}
