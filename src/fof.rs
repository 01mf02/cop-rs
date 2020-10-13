use crate::term::{App, Fresh, Subst, Term};
use num_bigint::BigUint;
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::Hash;
use tptp::syntax;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Form<C, V> {
    Atom(App<C, V>),
    EqTm(Term<C, V>, Term<C, V>),
    Neg(Box<Form<C, V>>),
    Conj(Box<Form<C, V>>, Box<Form<C, V>>),
    Disj(Box<Form<C, V>>, Box<Form<C, V>>),
    Impl(Box<Form<C, V>>, Box<Form<C, V>>),
    EqFm(Box<Form<C, V>>, Box<Form<C, V>>),
    Forall(V, Box<Form<C, V>>),
    Exists(V, Box<Form<C, V>>),
}

pub type SForm = Form<String, String>;

fn fold_right1<T>(mut vec: Vec<T>, f: impl Fn(T, T) -> T) -> Option<T> {
    match vec.pop() {
        None => None,
        Some(last) => Some(vec.into_iter().rev().fold(last, |acc, x| f(x, acc))),
    }
}

type Unfold<C, V> = Box<dyn Fn(Form<C, V>) -> (Change, Form<C, V>)>;
pub type SUnfold = Unfold<String, String>;

/// `true` if there has been a change, `false` if not
type Change = bool;

impl<C: Display, V: Display> Display for Form<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Form::*;
        match self {
            Forall(v, fm) => write!(f, "∀ {}. {}", v, fm),
            Exists(v, fm) => write!(f, "∃ {}. {}", v, fm),
            Conj(l, r) => write!(f, "({} ∧ {})", l, r),
            Disj(l, r) => write!(f, "({} ∨ {})", l, r),
            Impl(l, r) => write!(f, "({} ⇒ {})", l, r),
            EqFm(l, r) => write!(f, "({} ⇔ {})", l, r),
            EqTm(l, r) => write!(f, "{} = {}", l, r),
            Neg(fm) => write!(f, "¬ {}", fm),
            Atom(app) => write!(f, "{}", app),
        }
    }
}

impl<C, V> Form<C, V> {
    pub fn conjoin_right(frms: Vec<Self>) -> Option<Self> {
        fold_right1(frms, |x, acc| Form::Conj(Box::new(x), Box::new(acc)))
    }

    pub fn disjoin_right(frms: Vec<Self>) -> Option<Self> {
        fold_right1(frms, |x, acc| Form::Disj(Box::new(x), Box::new(acc)))
    }

    pub fn neg(self) -> Self {
        Self::Neg(Box::new(self))
    }

    pub fn conj(l: Self, r: Self) -> Self {
        Self::Conj(Box::new(l), Box::new(r))
    }

    pub fn disj(l: Self, r: Self) -> Self {
        Self::Disj(Box::new(l), Box::new(r))
    }

    pub fn forall(v: V, fm: Self) -> Self {
        Self::Forall(v, Box::new(fm))
    }

    pub fn exists(v: V, fm: Self) -> Self {
        Self::Exists(v, Box::new(fm))
    }

    pub fn is_atom(&self) -> bool {
        match self {
            Self::Atom(_) => true,
            _ => false,
        }
    }

    pub fn unfold_impl(self) -> (Change, Self) {
        use Form::*;
        match self {
            Impl(l, r) => (true, Disj(Box::new(Neg(l)), r)),
            Neg(x) => match *x {
                Impl(l, r) => (true, Conj(l, Box::new(Neg(r)))),
                x => (false, Neg(Box::new(x))),
            },
            x => (false, x),
        }
    }

    pub fn unfold_neg(self) -> (Change, Self) {
        use Form::*;
        match self {
            Neg(x) => match *x {
                Neg(t) => (true, *t),
                Forall(v, t) => (true, Self::exists(v, Neg(t))),
                Exists(v, t) => (true, Self::forall(v, Neg(t))),
                Conj(l, r) => (true, Self::disj(Neg(l), Neg(r))),
                Disj(l, r) => (true, Self::conj(Neg(l), Neg(r))),
                x => (false, Self::neg(x)),
            },
            x => (false, x),
        }
    }

    pub fn fix(self, f: &impl Fn(Self) -> (Change, Self)) -> Self {
        use Form::*;
        let (change, fm) = f(self);
        if change {
            fm.fix(f)
        } else {
            match fm {
                Forall(v, t) => Self::forall(v, t.fix(f)),
                Exists(v, t) => Self::exists(v, t.fix(f)),
                Conj(l, r) => Self::conj(l.fix(f), r.fix(f)),
                Disj(l, r) => Self::disj(l.fix(f), r.fix(f)),
                Impl(l, r) => Impl(Box::new(l.fix(f)), Box::new(r.fix(f))),
                EqFm(l, r) => EqFm(Box::new(l.fix(f)), Box::new(r.fix(f))),
                EqTm(l, r) => EqTm(l, r),
                Neg(t) => Self::neg(t.fix(f)),
                Atom(t) => Atom(t),
            }
        }
    }

    pub fn apply_unfolds(self, fs: &[Unfold<C, V>]) -> (Change, Self) {
        fs.iter().fold((false, self), |(change, x), f| {
            let (change_y, y) = f(x);
            (change | change_y, y)
        })
    }

    pub fn nnf(self) -> Self {
        self.fix(&|fm| fm.unfold_neg())
    }
}

impl<C: Clone, V: Clone> Form<C, V> {
    pub fn unfold_eqfm_nonclausal(self) -> (Change, Self) {
        use Form::*;
        match self {
            EqFm(l, r) => (true, Self::conj(Impl(l.clone(), r.clone()), Impl(r, l))),
            x => (false, x),
        }
    }

    /// Sort the formula by ascending number of paths.
    pub fn order(self) -> (Self, BigUint) {
        use num_traits::One;
        use Form::*;
        let order_bin = |l: Self, r: Self| {
            let (l, sl) = l.order();
            let (r, sr) = r.order();
            if sl > sr {
                ((r, sr), (l, sl))
            } else {
                ((l, sl), (r, sr))
            }
        };
        match self {
            Conj(l, r) => {
                let ((l, sl), (r, sr)) = order_bin(*l, *r);
                (Self::conj(l, r), sl * sr)
            }
            Disj(l, r) => {
                let ((l, sl), (r, sr)) = order_bin(*l, *r);
                (Self::disj(l, r), sl + sr)
            }
            a @ Atom(_) => (a, One::one()),
            Neg(a) if a.is_atom() => (Neg(a), One::one()),
            _ => panic!("unhandled formula"),
        }
    }

    // Expects nnf with no quantifiers
    pub fn cnf(self) -> Self {
        use Form::*;
        match self {
            Conj(l, r) => Self::conj(l.cnf(), r.cnf()),
            Disj(l, r) => match (*l, *r) {
                (Conj(a, b), c) => {
                    Self::conj(Self::disj(*a, c.clone()).cnf(), Self::disj(*b, c).cnf())
                }
                (a, Conj(b, c)) => {
                    Self::conj(Self::disj(a.clone(), *b).cnf(), Self::disj(a, *c).cnf())
                }
                (a, b) => match (a.cnf(), b.cnf()) {
                    (a @ Conj(_, _), b) | (a, b @ Conj(_, _)) => Self::disj(a, b).cnf(),
                    (a, b) => Self::disj(a, b),
                },
            },
            a @ Atom(_) => a,
            Neg(a) if a.is_atom() => Neg(a),
            _ => panic!("unhandled formula"),
        }
    }
}

impl<C: Clone, V: Clone + Eq + Hash> Form<C, V> {
    pub fn univar<W: Clone + Fresh>(self, mut map: HashMap<V, W>, st: &mut W::State) -> Form<C, W> {
        use Form::*;
        match self {
            Neg(fm) => Form::neg(fm.univar(map, st)),
            Atom(app) => Form::Atom(app.univar(map)),
            Conj(l, r) => Form::conj(l.univar(map.clone(), st), r.univar(map, st)),
            Disj(l, r) => Form::disj(l.univar(map.clone(), st), r.univar(map, st)),
            Forall(v, fm) => {
                let i = W::fresh(st);
                map.insert(v, i.clone());
                Form::forall(i, fm.univar(map, st))
            }
            Exists(v, fm) => {
                let i = W::fresh(st);
                map.insert(v, i.clone());
                Form::exists(i, fm.univar(map, st))
            }
            _ => panic!("unhandled formula"),
        }
    }
}

impl<C: Clone + Fresh, V: Clone + Eq + Hash> Form<C, V> {
    pub fn skolem_outer(self, uq: &mut Vec<V>, eq: &mut Subst<C, V>, st: &mut C::State) -> Self {
        use Form::*;
        match self {
            Atom(app) => Atom(app.subst(eq)),
            Neg(fm) => match *fm {
                Atom(_) => fm.skolem_outer(uq, eq, st),
                _ => panic!("not in negation normal form"),
            },
            Conj(l, r) => Form::conj(l.skolem_outer(uq, eq, st), r.skolem_outer(uq, eq, st)),
            Disj(l, r) => Form::disj(l.skolem_outer(uq, eq, st), r.skolem_outer(uq, eq, st)),
            Forall(v, fm) => {
                uq.push(v.clone());
                let fm = fm.skolem_outer(uq, eq, st);
                uq.pop();
                fm
            }
            Exists(v, fm) => {
                eq.insert(v.clone(), Term::skolem(st, uq.clone()));
                let fm = fm.skolem_outer(uq, eq, st);
                eq.remove(&v);
                fm
            }
            _ => panic!("unhandled formula"),
        }
    }
}

impl From<syntax::FofLogicFormula<'_>> for SForm {
    fn from(frm: syntax::FofLogicFormula) -> Self {
        use syntax::FofLogicFormula::*;
        match frm {
            Binary(b) => Self::from(b),
            Unary(_) => todo!(),
            Unitary(u) => Self::from(u),
        }
    }
}

impl From<syntax::FofQuantifiedFormula<'_>> for SForm {
    fn from(frm: syntax::FofQuantifiedFormula) -> Self {
        let mut f = Self::from(*frm.formula);
        for v in frm.bound.0.iter().rev().map(|v| v.to_string()) {
            use syntax::FofQuantifier::*;
            f = match frm.quantifier {
                Forall => Self::Forall(v, Box::new(f)),
                Exists => Self::Exists(v, Box::new(f)),
            }
        }
        f
    }
}

impl From<syntax::FofUnitFormula<'_>> for SForm {
    fn from(frm: syntax::FofUnitFormula) -> Self {
        use syntax::FofUnitFormula::*;
        match frm {
            Unitary(u) => Self::from(u),
            Unary(u) => Self::from(u),
        }
    }
}

impl From<syntax::FofUnaryFormula<'_>> for SForm {
    fn from(frm: syntax::FofUnaryFormula) -> Self {
        use syntax::FofUnaryFormula::*;
        match frm {
            // negation
            Unary(_negation, fuf) => Self::Neg(Box::new(Self::from(*fuf))),
            // term inequality
            InfixUnary(syntax::FofInfixUnary {
                left,
                op: syntax::InfixInequality,
                right,
            }) => Self::Neg(Box::new(Self::EqTm(Term::from(*left), Term::from(*right)))),
        }
    }
}

impl From<syntax::FofBinaryFormula<'_>> for SForm {
    fn from(frm: syntax::FofBinaryFormula) -> Self {
        use syntax::FofBinaryFormula::*;
        match frm {
            Nonassoc(fbn) => Self::from(fbn),
            Assoc(fba) => Self::from(fba),
        }
    }
}

impl From<syntax::FofBinaryNonassoc<'_>> for SForm {
    fn from(frm: syntax::FofBinaryNonassoc) -> Self {
        let left = Box::new(Self::from(*frm.left));
        let right = Box::new(Self::from(*frm.right));
        use syntax::NonassocConnective::*;
        match frm.op {
            LRImplies => Self::Impl(left, right),
            RLImplies => Self::Impl(right, left),
            Equivalent => Self::EqFm(left, right),
            NotEquivalent => Self::Neg(Box::new(Self::EqFm(left, right))),
            NotOr => Self::Neg(Box::new(Self::Disj(left, right))),
            NotAnd => Self::Neg(Box::new(Self::Conj(left, right))),
        }
    }
}

impl From<syntax::FofBinaryAssoc<'_>> for SForm {
    fn from(frm: syntax::FofBinaryAssoc) -> Self {
        use syntax::FofBinaryAssoc::*;
        match frm {
            Or(fof) => Self::disjoin_right(fof.0.into_iter().map(Self::from).collect()).unwrap(),
            And(faf) => Self::conjoin_right(faf.0.into_iter().map(Self::from).collect()).unwrap(),
        }
    }
}

impl From<syntax::FofUnitaryFormula<'_>> for SForm {
    fn from(frm: syntax::FofUnitaryFormula) -> Self {
        use syntax::FofUnitaryFormula::*;
        match frm {
            Parenthesised(flf) => Self::from(*flf),
            Quantified(fqf) => Self::from(fqf),
            Atomic(a) => Self::from(a),
        }
    }
}

impl From<syntax::FofPlainAtomicFormula<'_>> for SForm {
    fn from(frm: syntax::FofPlainAtomicFormula) -> Self {
        Self::Atom(App::from(frm.0))
    }
}

impl From<syntax::FofAtomicFormula<'_>> for SForm {
    fn from(frm: syntax::FofAtomicFormula) -> Self {
        use syntax::FofAtomicFormula::*;
        match frm {
            Plain(p) => Self::from(p),
            Defined(_) => todo!(),
            System(_) => todo!(),
        }
    }
}

impl From<syntax::FofFormula<'_>> for SForm {
    fn from(frm: syntax::FofFormula) -> Self {
        Self::from(frm.0)
    }
}
