use crate::term::{Args, Arity, Fresh, Term};
use core::fmt::{self, Display};
use core::hash::Hash;
use num_bigint::BigUint;
use std::collections::HashMap;
use tptp::{common, fof};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Form<C, V> {
    Atom(C, Args<C, V>),
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
            Atom(p, args) => write!(f, "{}{}", p, args),
        }
    }
}

impl<C, V> core::ops::Neg for Form<C, V> {
    type Output = Self;
    fn neg(self) -> Self {
        Self::Neg(Box::new(self))
    }
}

impl<C, V> core::ops::BitAnd for Form<C, V> {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Self::Conj(Box::new(self), Box::new(rhs))
    }
}

impl<C, V> core::ops::BitOr for Form<C, V> {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self::Disj(Box::new(self), Box::new(rhs))
    }
}

impl<C, V> Form<C, V> {
    pub fn conjoin_right(frms: Vec<Self>) -> Option<Self> {
        fold_right1(frms, |x, acc| Form::Conj(Box::new(x), Box::new(acc)))
    }

    pub fn disjoin_right(frms: Vec<Self>) -> Option<Self> {
        fold_right1(frms, |x, acc| Form::Disj(Box::new(x), Box::new(acc)))
    }

    pub fn forall(v: V, fm: Self) -> Self {
        Self::Forall(v, Box::new(fm))
    }

    pub fn exists(v: V, fm: Self) -> Self {
        Self::Exists(v, Box::new(fm))
    }

    pub fn imp(f1: Self, f2: Self) -> Self {
        Self::Impl(Box::new(f1), Box::new(f2))
    }

    pub fn eq_fm(f1: Self, f2: Self) -> Self {
        Self::EqFm(Box::new(f1), Box::new(f2))
    }

    pub fn foralls(vs: impl Iterator<Item = V>, fm: Self) -> Self {
        vs.fold(fm, |fm, v| Form::forall(v, fm))
    }

    pub fn add_premise(self, premise: Self) -> Self {
        match self {
            Form::Impl(a, b) => Form::imp(premise & *a, *b),
            _ => Form::imp(premise, self),
        }
    }

    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> W) -> Form<C, W> {
        use Form::*;
        let mv = &mut |v| Term::V(f(v));
        match self {
            EqTm(t1, t2) => EqTm(t1.map_vars(mv), t2.map_vars(mv)),
            Atom(p, args) => Atom(p, args.map_vars(mv)),
            Neg(fm) => -fm.map_vars(f),
            Forall(v, fm) => Form::forall(f(v), fm.map_vars(f)),
            Exists(v, fm) => Form::exists(f(v), fm.map_vars(f)),
            Conj(f1, f2) => f1.map_vars(f) & f2.map_vars(f),
            Disj(f1, f2) => f1.map_vars(f) | f2.map_vars(f),
            Impl(f1, f2) => Form::imp(f1.map_vars(f), f2.map_vars(f)),
            EqFm(f1, f2) => Form::eq_fm(f1.map_vars(f), f2.map_vars(f)),
        }
    }

    pub fn contains_eqtm(&self) -> bool {
        use Form::*;
        match self {
            EqTm(_, _) => true,
            Atom(_, _) => false,
            Neg(fm) | Exists(_, fm) | Forall(_, fm) => fm.contains_eqtm(),
            Conj(f1, f2) | Disj(f1, f2) | Impl(f1, f2) | EqFm(f1, f2) => {
                f1.contains_eqtm() || f2.contains_eqtm()
            }
        }
    }

    pub fn predicates(&self) -> Box<dyn Iterator<Item = (&C, Arity)> + '_> {
        use Form::*;
        match self {
            EqTm(_, _) => Box::new(core::iter::empty()),
            Atom(p, args) => Box::new(core::iter::once((p, args.len()))),
            Neg(fm) | Exists(_, fm) | Forall(_, fm) => fm.predicates(),
            Conj(f1, f2) | Disj(f1, f2) | Impl(f1, f2) | EqFm(f1, f2) => {
                Box::new(f1.predicates().chain(f2.predicates()))
            }
        }
    }

    pub fn constants(&self) -> Box<dyn Iterator<Item = (&C, Arity)> + '_> {
        use Form::*;
        match self {
            EqTm(t1, t2) => Box::new(t1.constants().chain(t2.constants())),
            Atom(_, args) => Box::new(args.constants()),
            Neg(fm) | Exists(_, fm) | Forall(_, fm) => fm.constants(),
            Conj(f1, f2) | Disj(f1, f2) | Impl(f1, f2) | EqFm(f1, f2) => {
                Box::new(f1.constants().chain(f2.constants()))
            }
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
                Conj(l, r) => (true, Neg(l) | Neg(r)),
                Disj(l, r) => (true, Neg(l) & Neg(r)),
                x => (false, -x),
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
                Conj(l, r) => l.fix(f) & r.fix(f),
                Disj(l, r) => l.fix(f) | r.fix(f),
                Impl(l, r) => Impl(Box::new(l.fix(f)), Box::new(r.fix(f))),
                EqFm(l, r) => EqFm(Box::new(l.fix(f)), Box::new(r.fix(f))),
                EqTm(l, r) => EqTm(l, r),
                Neg(t) => -t.fix(f),
                Atom(p, args) => Atom(p, args),
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

impl<C: Clone, V> Form<C, V> {
    pub fn unfold_eq_tm(self, eq: &C) -> (Change, Self) {
        // TODO: make this a bit prettier
        match self {
            Self::EqTm(t1, t2) => (
                true,
                Self::Atom(eq.clone(), vec![t1, t2].into_iter().collect()),
            ),
            x => (false, x),
        }
    }
}

impl<C: Clone, V: Clone> Form<C, V> {
    pub fn unfold_eqfm_nonclausal(self) -> (Change, Self) {
        use Form::*;
        match self {
            EqFm(l, r) => (true, Impl(l.clone(), r.clone()) & Impl(r, l)),
            x => (false, x),
        }
    }

    /// Sort the formula by ascending number of paths.
    pub fn order(self) -> (Self, BigUint) {
        use num_traits::One;
        use Form::*;
        let order_bin = |l: Self, r: Self| {
            let l = l.order();
            let r = r.order();
            if l.1 > r.1 {
                (r, l)
            } else {
                (l, r)
            }
        };
        match self {
            Conj(l, r) => {
                let ((l, sl), (r, sr)) = order_bin(*l, *r);
                (l & r, sl * sr)
            }
            Disj(l, r) => {
                let ((l, sl), (r, sr)) = order_bin(*l, *r);
                (l | r, sl + sr)
            }
            a if matches!(a, Self::Atom(_, _)) => (a, One::one()),
            Neg(a) if matches!(*a, Self::Atom(_, _)) => (Neg(a), One::one()),
            _ => panic!("unhandled formula"),
        }
    }

    // Expects nnf with no quantifiers
    pub fn cnf(self) -> Self {
        use Form::*;
        match self {
            Conj(l, r) => l.cnf() & r.cnf(),
            Disj(l, r) => match (*l, *r) {
                (Conj(a, b), r) => (*a | r.clone()).cnf() & (*b | r).cnf(),
                (l, Conj(b, c)) => (l.clone() | *b).cnf() & (l | *c).cnf(),
                (l, r) => match (l.cnf(), r.cnf()) {
                    (l @ Conj(_, _), r) | (l, r @ Conj(_, _)) => (l | r).cnf(),
                    (l, r) => l | r,
                },
            },
            a if matches!(a, Self::Atom(_, _)) => a,
            Neg(a) if matches!(*a, Self::Atom(_, _)) => Neg(a),
            _ => panic!("unhandled formula"),
        }
    }
}

impl<C: Clone, V: Clone + Eq + Hash> Form<C, V> {
    pub fn univar<W: Clone + Fresh>(self, mut map: HashMap<V, W>, st: &mut W::State) -> Form<C, W> {
        use Form::*;
        match self {
            Neg(fm) => -fm.univar(map, st),
            Atom(p, args) => Form::Atom(p, args.univar(map)),
            Conj(l, r) => l.univar(map.clone(), st) & r.univar(map, st),
            Disj(l, r) => l.univar(map.clone(), st) | r.univar(map, st),
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

pub struct SkolemState<C: Fresh, V> {
    universal: Vec<V>,
    existential: HashMap<V, Term<C, V>>,
    fresh: C::State,
}

impl<C: Fresh, V> SkolemState<C, V> {
    pub fn new(fresh: C::State) -> Self {
        Self {
            universal: Vec::new(),
            existential: HashMap::new(),
            fresh,
        }
    }
}

impl<C: Clone + Fresh, V: Clone + Eq + Hash> Form<C, V> {
    pub fn skolem_outer(self, st: &mut SkolemState<C, V>) -> Self {
        use Form::*;
        match self {
            Atom(p, args) => Atom(p, args.subst(&st.existential)),
            Neg(fm) => match *fm {
                Atom(_, _) => -fm.skolem_outer(st),
                _ => panic!("not in negation normal form"),
            },
            Conj(l, r) => l.skolem_outer(st) & r.skolem_outer(st),
            Disj(l, r) => l.skolem_outer(st) | r.skolem_outer(st),
            Forall(v, fm) => {
                st.universal.push(v);
                let fm = fm.skolem_outer(st);
                st.universal.pop();
                fm
            }
            Exists(v, fm) => {
                let skolem = Term::skolem(&mut st.fresh, st.universal.clone());
                assert!(!st.existential.contains_key(&v));
                st.existential.insert(v.clone(), skolem);
                let fm = fm.skolem_outer(st);
                st.existential.remove(&v);
                fm
            }
            _ => panic!("unhandled formula"),
        }
    }
}

impl From<fof::LogicFormula<'_>> for SForm {
    fn from(frm: fof::LogicFormula) -> Self {
        use fof::LogicFormula::*;
        match frm {
            Binary(b) => Self::from(b),
            Unary(_) => todo!(),
            Unitary(u) => Self::from(u),
        }
    }
}

impl From<fof::QuantifiedFormula<'_>> for SForm {
    fn from(frm: fof::QuantifiedFormula) -> Self {
        let mut f = Self::from(*frm.formula);
        for v in frm.bound.0.iter().rev().map(|v| v.to_string()) {
            use fof::Quantifier::*;
            f = match frm.quantifier {
                Forall => Self::Forall(v, Box::new(f)),
                Exists => Self::Exists(v, Box::new(f)),
            }
        }
        f
    }
}

impl From<fof::UnitFormula<'_>> for SForm {
    fn from(frm: fof::UnitFormula) -> Self {
        use fof::UnitFormula::*;
        match frm {
            Unitary(u) => Self::from(u),
            Unary(u) => Self::from(u),
        }
    }
}

impl From<fof::UnaryFormula<'_>> for SForm {
    fn from(frm: fof::UnaryFormula) -> Self {
        use fof::UnaryFormula::*;
        match frm {
            // negation
            Unary(_negation, fuf) => Self::Neg(Box::new(Self::from(*fuf))),
            // term inequality
            InfixUnary(fof::InfixUnary {
                left,
                op: common::InfixInequality,
                right,
            }) => -Self::EqTm(Term::from(*left), Term::from(*right)),
        }
    }
}

impl From<fof::BinaryFormula<'_>> for SForm {
    fn from(frm: fof::BinaryFormula) -> Self {
        use fof::BinaryFormula::*;
        match frm {
            Nonassoc(fbn) => Self::from(fbn),
            Assoc(fba) => Self::from(fba),
        }
    }
}

impl From<fof::BinaryNonassoc<'_>> for SForm {
    fn from(frm: fof::BinaryNonassoc) -> Self {
        let left = Box::new(Self::from(*frm.left));
        let right = Box::new(Self::from(*frm.right));
        use common::NonassocConnective::*;
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

impl From<fof::BinaryAssoc<'_>> for SForm {
    fn from(frm: fof::BinaryAssoc) -> Self {
        use fof::BinaryAssoc::*;
        match frm {
            Or(fof) => Self::disjoin_right(fof.0.into_iter().map(Self::from).collect()).unwrap(),
            And(faf) => Self::conjoin_right(faf.0.into_iter().map(Self::from).collect()).unwrap(),
        }
    }
}

impl From<fof::UnitaryFormula<'_>> for SForm {
    fn from(frm: fof::UnitaryFormula) -> Self {
        use fof::UnitaryFormula::*;
        match frm {
            Parenthesised(flf) => Self::from(*flf),
            Quantified(fqf) => Self::from(fqf),
            Atomic(a) => Self::from(*a),
        }
    }
}

impl From<fof::PlainAtomicFormula<'_>> for SForm {
    fn from(frm: fof::PlainAtomicFormula) -> Self {
        use fof::PlainTerm::*;
        match frm.0 {
            Constant(c) => Self::Atom(c.to_string(), Args::new()),
            Function(f, args) => Self::Atom(f.to_string(), Args::from(*args)),
        }
    }
}

impl From<fof::DefinedAtomicFormula<'_>> for SForm {
    fn from(frm: fof::DefinedAtomicFormula) -> Self {
        use fof::DefinedAtomicFormula::*;
        match frm {
            Plain(_) => todo!(),
            Infix(i) => Form::EqTm(Term::from(*i.left), Term::from(*i.right)),
        }
    }
}

impl From<fof::AtomicFormula<'_>> for SForm {
    fn from(frm: fof::AtomicFormula) -> Self {
        use fof::AtomicFormula::*;
        match frm {
            Plain(p) => Self::from(p),
            Defined(d) => Self::from(d),
            System(_) => todo!(),
        }
    }
}

impl From<fof::Formula<'_>> for SForm {
    fn from(frm: fof::Formula) -> Self {
        Self::from(frm.0)
    }
}
