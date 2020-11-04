use crate::term::{Args, Arity, Fresh, Term};
use core::fmt::{self, Display};
use core::hash::Hash;
use core::ops::Neg;
use num_bigint::BigUint;
use std::collections::HashMap;
use tptp::{common, fof};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Form<P, C, V> {
    Atom(P, Args<C, V>),
    EqTm(Term<C, V>, Term<C, V>),
    Neg(Box<Form<P, C, V>>),
    Bin(Box<Form<P, C, V>>, Op, Box<Form<P, C, V>>),
    Quant(Quantifier, V, Box<Form<P, C, V>>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Op {
    Conj,
    Disj,
    Impl,
    EqFm,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Quantifier {
    Forall,
    Exists,
}

impl Neg for Quantifier {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Quantifier::Forall => Quantifier::Exists,
            Quantifier::Exists => Quantifier::Forall,
        }
    }
}

impl<P: Display, C: Display, V: Display> Display for Form<P, C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Form::*;
        match self {
            Atom(p, args) => write!(f, "{}{}", p, args),
            EqTm(l, r) => write!(f, "{} = {}", l, r),
            Neg(fm) => write!(f, "¬ {}", fm),
            Bin(l, o, r) => write!(f, "({} {} {})", l, o, r),
            Quant(q, v, fm) => write!(f, "{} {}. {}", q, v, fm),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Conj => write!(f, "∧"),
            Op::Disj => write!(f, "∨"),
            Op::Impl => write!(f, "⇒"),
            Op::EqFm => write!(f, "⇔"),
        }
    }
}

impl Display for Quantifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Quantifier::Forall => write!(f, "∀"),
            Quantifier::Exists => write!(f, "∃"),
        }
    }
}

impl<P, C, V> Neg for Form<P, C, V> {
    type Output = Self;
    fn neg(self) -> Self {
        Self::Neg(Box::new(self))
    }
}

impl<P, C, V> core::ops::BitAnd for Form<P, C, V> {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Self::Bin(Box::new(self), Op::Conj, Box::new(rhs))
    }
}

impl<P, C, V> core::ops::BitOr for Form<P, C, V> {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self::Bin(Box::new(self), Op::Disj, Box::new(rhs))
    }
}

/// `true` if there has been a change, `false` if not
type Change = bool;

pub type Unfold<T> = Box<dyn Fn(T) -> (Change, T)>;

fn fold_right1<T>(mut vec: Vec<T>, f: impl Fn(T, T) -> T) -> Option<T> {
    match vec.pop() {
        None => None,
        Some(last) => Some(vec.into_iter().rev().fold(last, |acc, x| f(x, acc))),
    }
}

impl<P, C, V> Form<P, C, V> {
    pub fn bin(l: Self, o: Op, r: Self) -> Self {
        Self::Bin(Box::new(l), o, Box::new(r))
    }

    pub fn bins(fms: Vec<Self>, op: Op) -> Option<Self> {
        fold_right1(fms, |x, acc| Self::bin(x, op, acc))
    }

    pub fn imp(l: Self, r: Self) -> Self {
        Self::bin(l, Op::Impl, r)
    }

    pub fn quant(q: Quantifier, v: V, fm: Self) -> Self {
        Self::Quant(q, v, Box::new(fm))
    }

    pub fn quants(q: Quantifier, vs: impl Iterator<Item = V>, fm: Self) -> Self {
        vs.fold(fm, |fm, v| Self::quant(q, v, fm))
    }

    pub fn forall(v: V, fm: Self) -> Self {
        Self::quant(Quantifier::Forall, v, fm)
    }

    pub fn foralls(vs: impl Iterator<Item = V>, fm: Self) -> Self {
        vs.fold(fm, |fm, v| Form::forall(v, fm))
    }

    pub fn add_premise(self, premise: Self) -> Self {
        match self {
            Form::Bin(a, Op::Impl, b) => Self::imp(premise & *a, *b),
            _ => Self::imp(premise, self),
        }
    }

    pub fn map_predicates<Q>(self, f: &impl Fn(P) -> Q) -> Form<Q, C, V> {
        use Form::*;
        match self {
            Atom(p, args) => Atom(f(p), args),
            EqTm(t1, t2) => EqTm(t1, t2),
            Neg(fm) => -fm.map_predicates(f),
            Bin(l, o, r) => Form::bin(l.map_predicates(f), o, r.map_predicates(f)),
            Quant(q, v, fm) => Form::quant(q, v, fm.map_predicates(f)),
        }
    }

    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> W) -> Form<P, C, W> {
        use Form::*;
        let mv = &mut |v| Term::V(f(v));
        match self {
            Atom(p, args) => Atom(p, args.map_vars(mv)),
            EqTm(t1, t2) => EqTm(t1.map_vars(mv), t2.map_vars(mv)),
            Neg(fm) => -fm.map_vars(f),
            Bin(l, o, r) => Form::bin(l.map_vars(f), o, r.map_vars(f)),
            Quant(q, v, fm) => Form::quant(q, f(v), fm.map_vars(f)),
        }
    }

    pub fn subforms(&self) -> Box<dyn Iterator<Item = &Form<P, C, V>> + '_> {
        use core::iter::once;
        use Form::*;
        match self {
            Atom(_, _) | EqTm(_, _) => Box::new(once(self)),
            Neg(fm) | Quant(_, _, fm) => Box::new(once(self).chain(fm.subforms())),
            Bin(l, _, r) => Box::new(once(self).chain(l.subforms()).chain(r.subforms())),
        }
    }

    pub fn predicates(&self) -> impl Iterator<Item = (&P, Arity)> {
        self.subforms().filter_map(|fm| match fm {
            Self::Atom(p, args) => Some((p, args.len())),
            _ => None,
        })
    }

    pub fn constants(&self) -> impl Iterator<Item = (&C, Arity)> {
        self.subforms()
            .map(|fm| match fm {
                Self::Atom(_, args) => Box::new(args.constants()),
                // TODO: this cast is ugly ...
                Self::EqTm(l, r) => {
                    Box::new(l.constants().chain(r.constants())) as Box<dyn Iterator<Item = _>>
                }
                _ => Box::new(std::iter::empty()),
            })
            .flatten()
    }

    /// Sort the formula by ascending number of paths.
    pub fn order(self) -> (Self, BigUint) {
        use num_traits::One;
        use Form::*;
        match self {
            Bin(l, op, r) if matches!(op, Op::Conj | Op::Disj) => {
                let l = l.order();
                let r = r.order();
                let ((l, sl), (r, sr)) = if l.1 > r.1 { (r, l) } else { (l, r) };
                (Self::bin(l, op, r), sl * sr)
            }
            a if matches!(a, Self::Atom(_, _)) => (a, One::one()),
            Neg(a) if matches!(*a, Self::Atom(_, _)) => (Neg(a), One::one()),
            _ => panic!("unhandled formula"),
        }
    }

    pub fn fix(self, f: &impl Fn(Self) -> (Change, Self)) -> Self {
        use Form::*;
        let (change, fm) = f(self);
        // TODO: eliminate recursion?
        if change {
            fm.fix(f)
        } else {
            match fm {
                Atom(_, _) | EqTm(_, _) => fm,
                Quant(q, v, t) => Self::quant(q, v, t.fix(f)),
                Bin(l, o, r) => Self::bin(l.fix(f), o, r.fix(f)),
                Neg(t) => -t.fix(f),
            }
        }
    }

    pub fn unfold_impl(self) -> (Change, Self) {
        use Form::*;
        match self {
            Bin(l, Op::Impl, r) => (true, -*l | *r),
            Neg(x) => match *x {
                Bin(l, Op::Impl, r) => (true, *l & -*r),
                x => (false, -x),
            },
            x => (false, x),
        }
    }

    pub fn unfold_neg(self) -> (Change, Self) {
        use Form::*;
        match self {
            Neg(x) => match *x {
                Neg(t) => (true, *t),
                Bin(l, Op::Conj, r) => (true, -*l | -*r),
                Bin(l, Op::Disj, r) => (true, -*l & -*r),
                Quant(q, v, t) => (true, Self::quant(-q, v, -*t)),
                x => (false, -x),
            },
            x => (false, x),
        }
    }

    pub fn apply_unfolds(self, fs: &[Unfold<Self>]) -> (Change, Self) {
        fs.iter().fold((false, self), |(change, x), f| {
            let (change_y, y) = f(x);
            (change | change_y, y)
        })
    }

    pub fn nnf(self) -> Self {
        self.fix(&|fm| fm.unfold_neg())
    }
}

impl<P: Clone, C, V> Form<P, C, V> {
    pub fn unfold_eq_tm(self, eq: &P) -> (Change, Self) {
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

impl<P: Clone, C: Clone, V: Clone> Form<P, C, V> {
    pub fn unfold_eqfm_nonclausal(self) -> (Change, Self) {
        use Form::Bin;
        match self {
            Bin(l, Op::EqFm, r) => (true, Self::imp(*l.clone(), *r.clone()) & Self::imp(*r, *l)),
            x => (false, x),
        }
    }

    // Expects nnf with no quantifiers
    pub fn cnf(self) -> Self {
        use Form::*;
        use Op::{Conj, Disj};
        match self {
            Bin(l, Conj, r) => l.cnf() & r.cnf(),
            Bin(l, Disj, r) => match (*l, *r) {
                (Bin(a, Conj, b), r) => (*a | r.clone()).cnf() & (*b | r).cnf(),
                (l, Bin(b, Conj, c)) => (l.clone() | *b).cnf() & (l | *c).cnf(),
                (l, r) => match (l.cnf(), r.cnf()) {
                    (l @ Bin(_, Conj, _), r) | (l, r @ Bin(_, Conj, _)) => (l | r).cnf(),
                    (l, r) => l | r,
                },
            },
            a if matches!(a, Self::Atom(_, _)) => a,
            Neg(a) if matches!(*a, Self::Atom(_, _)) => Neg(a),
            _ => panic!("unhandled formula"),
        }
    }
}

impl<P, C, V: Clone + Eq + Hash> Form<P, C, V> {
    pub fn fresh_vars<W>(self, map: &mut HashMap<V, W>, st: &mut W::State) -> Form<P, C, W>
    where
        W: Clone + Fresh,
    {
        use Form::*;
        match self {
            Atom(p, args) => Form::Atom(p, args.fresh_vars(map, st)),
            EqTm(l, r) => Form::EqTm(l.fresh_vars(map, st), r.fresh_vars(map, st)),
            Neg(fm) => -fm.fresh_vars(map, st),
            Bin(l, o, r) => Form::bin(l.fresh_vars(map, st), o, r.fresh_vars(map, st)),
            Quant(q, v, fm) => {
                let i = W::fresh(st);
                let old = map.insert(v.clone(), i.clone());
                let fm = fm.fresh_vars(map, st);
                match old {
                    Some(old) => map.insert(v, old),
                    None => map.remove(&v),
                };
                Form::quant(q, i, fm)
            }
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

impl<P, C: Clone + Fresh, V: Clone + Eq + Hash> Form<P, C, V> {
    pub fn skolem_outer(self, st: &mut SkolemState<C, V>) -> Self {
        use Form::*;
        use Op::{Conj, Disj};
        match self {
            Atom(p, args) => Atom(p, args.subst(&st.existential)),
            Neg(fm) if matches!(*fm, Atom(_, _)) => -fm.skolem_outer(st),
            Bin(l, o, r) if matches!(o, Conj | Disj) => {
                Self::bin(l.skolem_outer(st), o, r.skolem_outer(st))
            }
            Quant(Quantifier::Forall, v, fm) => {
                st.universal.push(v);
                let fm = fm.skolem_outer(st);
                st.universal.pop();
                fm
            }
            Quant(Quantifier::Exists, v, fm) => {
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

pub type SForm = Form<String, String, String>;

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
        let q = Quantifier::from(frm.quantifier);
        let vs = frm.bound.0.iter().rev().map(|v| v.to_string());
        vs.fold(Self::from(*frm.formula), |fm, v| Self::quant(q, v, fm))
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
            Unary(_negation, fuf) => -Self::from(*fuf),
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
            LRImplies => Self::Bin(left, Op::Impl, right),
            RLImplies => Self::Bin(right, Op::Impl, left),
            Equivalent => Self::Bin(left, Op::EqFm, right),
            NotEquivalent => -Self::Bin(left, Op::EqFm, right),
            NotOr => -(*left | *right),
            NotAnd => -(*left & *right),
        }
    }
}

impl From<fof::BinaryAssoc<'_>> for SForm {
    fn from(frm: fof::BinaryAssoc) -> Self {
        use fof::BinaryAssoc::*;
        match frm {
            Or(fms) => Self::bins(fms.0.into_iter().map(Self::from).collect(), Op::Disj),
            And(fms) => Self::bins(fms.0.into_iter().map(Self::from).collect(), Op::Conj),
        }
        .unwrap()
    }
}

impl From<fof::Quantifier> for Quantifier {
    fn from(q: fof::Quantifier) -> Self {
        use fof::Quantifier::*;
        match q {
            Forall => Self::Forall,
            Exists => Self::Exists,
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
