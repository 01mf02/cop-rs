use crate::fof::{Fof, FofAtom, OpA};
use crate::term::{Args, Arity, Term};
use alloc::vec::Vec;

impl<P, C> Fof<FofAtom<P, C, usize>, usize> {
    fn eq_refl() -> Self {
        let xx = Self::eqtm(Term::V(0), Term::V(0));
        Self::forall(0, xx)
    }

    fn eq_sym() -> Self {
        let vars = [0, 1].iter().rev().cloned();
        let xy = Self::eqtm(Term::V(0), Term::V(1));
        let yx = Self::eqtm(Term::V(1), Term::V(0));
        Self::foralls(vars, Self::imp(xy, yx))
    }

    fn eq_trans() -> Self {
        let vars = [0, 1, 2].iter().rev().cloned();
        let xy = Self::eqtm(Term::V(0), Term::V(1));
        let yz = Self::eqtm(Term::V(1), Term::V(2));
        let xz = Self::eqtm(Term::V(0), Term::V(2));
        Self::foralls(vars, Self::imp(xy & yz, xz))
    }

    fn eq_subst<F>(arity: Arity, f: F) -> Option<Self>
    where
        F: FnOnce(Self, Args<C, usize>, Args<C, usize>) -> Self,
    {
        if arity == 0 {
            return None;
        }
        // construct `0 = 1 & (2 = 3 & (..))`, consisting of `arity` conjuncts
        let eq = (0..arity).map(|v| Self::eqtm(Term::V(2 * v), Term::V(1 + 2 * v)));
        let eq = Self::BinA(OpA::Conj, eq.collect());
        let al = (0..arity).map(|v| Term::V(2 * v)).collect();
        let ar = (0..arity).map(|v| Term::V(1 + 2 * v)).collect();
        Some(Self::foralls((0..2 * arity).rev(), f(eq, al, ar)))
    }
}

impl<P: Clone, C: Clone> Fof<FofAtom<P, C, usize>, usize> {
    /// Produce the substitution axiom for the given constant of arity.
    pub fn eq_constant(c: &C, arity: Arity) -> Option<Self> {
        let app = |args| Term::C(c.clone(), args);
        Self::eq_subst(arity, |eqs, al, ar| {
            Self::imp(eqs, Self::eqtm(app(al), app(ar)))
        })
    }

    /// Produce the substitution axiom for the given predicate of arity.
    pub fn eq_predicate(p: &P, arity: Arity) -> Option<Self> {
        let app = |args| Self::atom(p.clone(), args);
        Self::eq_subst(arity, |eqs, al, ar| Self::imp(eqs & app(al), app(ar)))
    }

    /// Produce equality axioms from a sequence of predicates and constants.
    ///
    /// Assume that `p1, ..., pn` and `c1, ..., cm` are
    /// substitution axioms for the given predicates and constants.
    /// Then the final output will be
    /// `((refl & sym & trans) & p1 & ... & pn) & c1 & ... & cm`,
    /// where the conjunction is associated to the right.
    pub fn eq_axioms(preds: Vec<(&P, Arity)>, consts: Vec<(&C, Arity)>) -> Self {
        let init = Self::eq_refl() & (Self::eq_sym() & Self::eq_trans());
        let p = preds
            .into_iter()
            .filter_map(|(p, arity)| Self::eq_predicate(p, arity));
        let c = consts
            .into_iter()
            .filter_map(|(c, arity)| Self::eq_constant(c, arity));
        (init & Self::binas(OpA::Conj, p)) & Self::binas(OpA::Conj, c)
    }
}

#[test]
fn eq_constant() {
    let eqtm = Fof::eqtm;
    use Term::{C, V};

    // ![0, 1, 2, 3]: (0 = 1 & 2 = 3) => f(0, 2) = f(1, 3)
    let f = "f";
    let premise = eqtm(V(0), V(1)) & eqtm(V(2), V(3));
    let concl = eqtm(
        C(f, Args::from([V(0), V(2)])),
        C(f, Args::from([V(1), V(3)])),
    );
    let out = Fof::foralls((0..4).rev(), Fof::imp(premise, concl));
    assert_eq!(Fof::eq_constant(&f, 2), Some::<Fof<FofAtom<(), _, _>, _>>(out));

    // for a nullary function symbol, there is no substitution axiom
    assert_eq!(Fof::eq_constant(&f, 0), None::<Fof<FofAtom<(), _, _>, _>>);
}
