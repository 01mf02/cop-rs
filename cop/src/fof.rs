use crate::change::{self, Change};
use crate::term::{Args, Arity, Fresh, Term};
use crate::Lit;
use alloc::{boxed::Box, vec::Vec};
use core::fmt::{self, Display};
use core::hash::Hash;
use core::ops::Neg;
use hashbrown::HashMap;
use num_bigint::BigUint;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Form<P, C, V> {
    Atom(P, Args<C, V>),
    EqTm(Term<C, V>, Term<C, V>),
    Neg(Box<Form<P, C, V>>),
    /// binary operation
    Bin(Box<Form<P, C, V>>, Op, Box<Form<P, C, V>>),
    /// associative binary operation
    BinA(OpA, Vec<Form<P, C, V>>),
    Quant(Quantifier, V, Box<Form<P, C, V>>),
}

/// Quantified negation-normal form.
pub enum QNnf<L, V> {
    Lit(L),
    BinA(OpA, Vec<QNnf<L, V>>),
    Quant(Quantifier, V, Box<QNnf<L, V>>),
}

/// Negation-normal form, implicitly universally quantified.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Nnf<L> {
    Lit(L),
    BinA(OpA, Vec<Nnf<L>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cnf<L> {
    Conj(Vec<Cnf<L>>),
    Disj(Dnf<L>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Dnf<L> {
    Lit(L),
    Disj(Vec<Dnf<L>>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Op {
    Impl,
    EqFm,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OpA {
    Conj,
    Disj,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Quantifier {
    Forall,
    Exists,
}

impl Neg for OpA {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Self::Conj => Self::Disj,
            Self::Disj => Self::Conj,
        }
    }
}

impl Neg for Quantifier {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Self::Forall => Self::Exists,
            Self::Exists => Self::Forall,
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
            BinA(o, fms) => o.fmt_args(fms, f),
            Quant(q, v, fm) => write!(f, "{} {}. {}", q, v, fm),
        }
    }
}

impl<L: Display, V: Display> Display for QNnf<L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use QNnf::*;
        match self {
            Lit(lit) => lit.fmt(f),
            BinA(o, fms) => o.fmt_args(fms, f),
            Quant(q, v, fm) => write!(f, "{} {}. {}", q, v, fm),
        }
    }
}

impl<L: Display> Display for Nnf<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Nnf::*;
        match self {
            Lit(lit) => lit.fmt(f),
            BinA(o, fms) => o.fmt_args(fms, f),
        }
    }
}

impl<L: Display> Display for Cnf<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Cnf::*;
        match self {
            Conj(fms) => OpA::Conj.fmt_args(fms, f),
            Disj(disj) => disj.fmt(f),
        }
    }
}

impl<L: Display> Display for Dnf<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Dnf::*;
        match self {
            Lit(lit) => lit.fmt(f),
            Disj(fms) => OpA::Disj.fmt_args(fms, f),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Impl => write!(f, "⇒"),
            Op::EqFm => write!(f, "⇔"),
        }
    }
}

impl Display for OpA {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OpA::Conj => write!(f, "∧"),
            OpA::Disj => write!(f, "∨"),
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

impl<L: Neg<Output = L>, V> Neg for QNnf<L, V> {
    type Output = Self;
    fn neg(self) -> Self {
        match self {
            Self::Lit(l) => Self::Lit(-l),
            Self::BinA(op, fms) => Self::BinA(-op, fms.into_iter().map(|fm| -fm).collect()),
            Self::Quant(q, v, fm) => Self::Quant(-q, v, Box::new(-*fm)),
        }
    }
}

impl<P, C, V> core::ops::BitAnd for Form<P, C, V> {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Self::bina(self, OpA::Conj, rhs)
    }
}

impl<P, C, V> core::ops::BitOr for Form<P, C, V> {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self::bina(self, OpA::Disj, rhs)
    }
}

impl<L> core::ops::BitAnd for Cnf<L> {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        match rhs {
            Self::Conj(fms) => join(self, fms, Self::Conj),
            _ => Self::Conj(Vec::from([self, rhs])),
        }
    }
}

impl<L: Clone> core::ops::BitOr for Cnf<L> {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Conj(lc), r) => Self::conjs(lc.into_iter().map(|ln| ln | r.clone())),
            (l, Self::Conj(rc)) => Self::conjs(rc.into_iter().map(|rn| l.clone() | rn)),
            (Self::Disj(ld), Self::Disj(rd)) => Self::Disj(ld | rd),
        }
    }
}

impl<L> core::ops::BitOr for Dnf<L> {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        match rhs {
            Self::Disj(fms) => join(self, fms, Self::Disj),
            _ => Self::Disj(Vec::from([self, rhs])),
        }
    }
}

impl<P, C, V> Form<P, C, V> {
    pub fn bin(l: Self, o: Op, r: Self) -> Self {
        Self::Bin(Box::new(l), o, Box::new(r))
    }

    pub fn bina(l: Self, o: OpA, r: Self) -> Self {
        match r {
            Self::BinA(op, fms) if o == op => join(l, fms, |fms| Self::BinA(o, fms)),
            _ => Self::BinA(o, Vec::from([l, r])),
        }
    }

    /// For formulas f1, .., fn, return f1 o (... o fn).
    pub fn binas(o: OpA, fms: impl DoubleEndedIterator<Item = Self>) -> Self {
        fms.rev()
            .reduce(|acc, x| Self::bina(x, o, acc))
            .unwrap_or_else(|| Self::BinA(o, Vec::new()))
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

    pub fn map_predicates<Q>(self, f: &mut impl FnMut(P) -> Q) -> Form<Q, C, V> {
        use Form::*;
        match self {
            Atom(p, args) => Atom(f(p), args),
            EqTm(t1, t2) => EqTm(t1, t2),
            Neg(fm) => -fm.map_predicates(f),
            Bin(l, o, r) => Form::bin(l.map_predicates(f), o, r.map_predicates(f)),
            BinA(o, fms) => BinA(o, fms.into_iter().map(|fm| fm.map_predicates(f)).collect()),
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
            BinA(o, fms) => BinA(o, fms.into_iter().map(|fm| fm.map_vars(f)).collect()),
            Quant(q, v, fm) => Form::quant(q, f(v), fm.map_vars(f)),
        }
    }

    fn map_form(self, f: impl Fn(Self) -> Self) -> Self {
        use Form::*;
        match self {
            Atom(_, _) | EqTm(_, _) => self,
            Neg(fm) => -f(*fm),
            Bin(l, o, r) => Self::bin(f(*l), o, f(*r)),
            BinA(o, fms) => BinA(o, fms.into_iter().map(|fm| f(fm)).collect()),
            Quant(q, v, fm) => Self::quant(q, v, f(*fm)),
        }
    }

    pub fn subforms(&self) -> Box<dyn Iterator<Item = &Form<P, C, V>> + '_> {
        use core::iter::once;
        use Form::*;
        match self {
            Atom(_, _) | EqTm(_, _) => Box::new(once(self)),
            Neg(fm) | Quant(_, _, fm) => Box::new(once(self).chain(fm.subforms())),
            Bin(l, _, r) => Box::new(once(self).chain(l.subforms()).chain(r.subforms())),
            BinA(_, fms) => Box::new(once(self).chain(fms.iter().flat_map(|fm| fm.subforms()))),
        }
    }
}

impl<P: Clone + Neg<Output = P>, C: Clone, V: Clone> Form<P, C, V> {
    pub fn qnnf(self) -> QNnf<Lit<P, C, V>, V> {
        match self {
            // leanCoP-specific
            Self::Bin(l, Op::EqFm, r) => ((*l.clone() & *r.clone()) | (-*l & -*r)).qnnf(),
            Self::Bin(l, Op::Impl, r) => (-*l | *r).qnnf(),
            Self::Neg(x) => match *x {
                // leanCoP-specific
                Self::Bin(l, Op::EqFm, r) => ((*l.clone() & -*r.clone()) | (-*l & *r)).qnnf(),
                Self::Bin(l, Op::Impl, r) => (*l & -*r).qnnf(),
                Self::Atom(p, args) => QNnf::Lit(Lit::new(-p, args)),
                Self::Neg(t) => t.qnnf(),
                Self::BinA(op, fms) => {
                    QNnf::BinA(-op, fms.into_iter().map(|fm| (-fm).qnnf()).collect())
                }
                Self::Quant(q, v, t) => QNnf::Quant(-q, v, Box::new((-*t).qnnf())),
                _ => panic!("unhandled formula"),
            },
            Self::Atom(p, args) => QNnf::Lit(Lit::new(p, args)),
            Self::BinA(op, fms) => QNnf::BinA(op, fms.into_iter().map(|fm| fm.qnnf()).collect()),
            Self::Quant(q, v, t) => QNnf::Quant(q, v, Box::new(t.qnnf())),
            _ => panic!("unhandled formula"),
        }
    }
}

impl<L> Nnf<L> {
    /// Sort the formula by ascending number of paths.
    pub fn order(self) -> (Self, BigUint) {
        use num_traits::{One, Zero};
        use Nnf::*;
        match self {
            BinA(op, fms) => {
                let neutral = match op {
                    OpA::Conj => One::one(),
                    OpA::Disj => Zero::zero(),
                };
                let comb = match op {
                    OpA::Conj => |l, r| l * r,
                    OpA::Disj => |l, r| l + r,
                };
                let fms = fms.into_iter().rev().map(|fm| fm.order());
                fms.reduce(|acc, x| {
                    let (l, r) = if x.1 > acc.1 {
                        (acc.0, x.0)
                    } else {
                        (x.0, acc.0)
                    };
                    (Self::bina(l, op, r), comb(acc.1, x.1))
                })
                .unwrap_or_else(|| (BinA(op, Vec::new()), neutral))
            }
            Self::Lit(_) => (self, One::one()),
        }
    }
}

impl<P, C, V> Form<P, C, V> {
    pub fn fix(self, f: &impl Fn(Self) -> (Change, Self)) -> Self {
        change::fix(self, f).map_form(|fm| fm.fix(f))
    }
}

impl<P: Clone, C, V> Form<P, C, V> {
    pub fn unfold_eq_tm(self, eq: P) -> (Change, Self) {
        match self {
            Self::EqTm(t1, t2) => (true, Self::Atom(eq.clone(), Args::from([t1, t2]))),
            x => (false, x),
        }
    }
}

impl<P: Clone, C: Clone, V: Clone> Form<P, C, V> {
    pub fn mark_impl(self, fm: &Self) -> (Change, Self) {
        match self {
            Form::Bin(a, Op::Impl, c) => (true, Form::imp(*a & fm.clone(), fm.clone() & *c)),
            _ => (false, self),
        }
    }

    /*
    /// Unfold logical equivalence with a disjunction of conjunctions.
    ///
    /// Used in (nondefinitional) leanCoP.
    pub fn unfold_eqfm_disj_conj(self) -> (Change, Self) {
        use Form::{Bin, Neg};
        use Op::EqFm;
        match self {
            Bin(l, EqFm, r) => (true, (*l.clone() & *r.clone()) | (-*l & -*r)),
            Neg(x) => match *x {
                Bin(l, EqFm, r) => (true, (*l.clone() & -*r.clone()) | (-*l & *r)),
                x => (false, -x),
            },
            x => (false, x),
        }
    }

    /// Unfold logical equivalence with a conjunction of implications.
    ///
    /// Used in nanoCoP.
    pub fn unfold_eqfm_conj_impl(self) -> (Change, Self) {
        use Form::Bin;
        match self {
            Bin(l, Op::EqFm, r) => (true, Self::imp(*l.clone(), *r.clone()) & Self::imp(*r, *l)),
            x => (false, x),
        }
    }
    */
}

impl<L> Nnf<L> {
    // TODO: used in `order`, really necessary?
    pub fn bina(l: Self, o: OpA, r: Self) -> Self {
        match r {
            Self::BinA(op, fms) if o == op => join(l, fms, |fms| Self::BinA(o, fms)),
            _ => Self::BinA(o, Vec::from([l, r])),
        }
    }

    // TODO: used in `cnf`, really necessary?
    /// For formulas f1, .., fn, return f1 o (... o fn).
    pub fn binas(o: OpA, fms: impl DoubleEndedIterator<Item = Self>) -> Self {
        fms.rev()
            .reduce(|acc, x| Self::bina(x, o, acc))
            .unwrap_or_else(|| Self::BinA(o, Vec::new()))
    }

    pub fn map_literals<M>(self, f: &mut impl FnMut(L) -> M) -> Nnf<M> {
        match self {
            Self::Lit(l) => Nnf::Lit(f(l)),
            Self::BinA(op, fms) => {
                Nnf::BinA(op, fms.into_iter().map(|fm| fm.map_literals(f)).collect())
            }
        }
    }
}

impl<L: Clone> Nnf<L> {
    /// CNF of the disjunction of two formulas.
    pub fn cnf_disj(self, other: Self) -> Cnf<L> {
        use Nnf::BinA;
        match (self, other) {
            (BinA(OpA::Conj, lc), r) => Cnf::conjs(lc.into_iter().map(|ln| ln.cnf_disj(r.clone()))),
            (l, BinA(OpA::Conj, rc)) => Cnf::conjs(rc.into_iter().map(|rn| l.clone().cnf_disj(rn))),
            (l, r) => l.cnf() | r.cnf(),
        }
    }

    /// CNF of an NNF with no quantifiers.
    pub fn cnf(self) -> Cnf<L> {
        use Nnf::*;
        match self {
            BinA(OpA::Conj, fms) => Cnf::conjs(fms.into_iter().map(|fm| fm.cnf())),
            BinA(OpA::Disj, fms) => {
                let mut fms = fms.into_iter();
                match fms.next() {
                    None => Cnf::Disj(Dnf::Disj(Vec::new())),
                    Some(fm1) => fm1.cnf_disj(Self::binas(OpA::Disj, fms)),
                }
            }
            Lit(l) => Cnf::Disj(Dnf::Lit(l)),
        }
    }
}

impl<P: Eq, C: Eq, V> Form<P, C, V> {
    /// Corresponds to leanCoP's `collect_predfunc`.
    pub fn predconst_unique(&self) -> (Vec<(&P, Arity)>, Vec<(&C, Arity)>) {
        use Form::*;
        match self {
            Atom(p, args) => (Vec::from([(p, args.len())]), args.const_unique()),
            EqTm(l, r) => {
                let mut cl = l.const_unique();
                let cr = r.const_unique();
                crate::union1(&mut cl, cr);
                (Vec::new(), cl)
            }
            Bin(l, _, r) => {
                let (mut pl, mut cl) = l.predconst_unique();
                let (pr, cr) = r.predconst_unique();
                crate::union1(&mut pl, pr);
                crate::union1(&mut cl, cr);
                (pl, cl)
            }
            BinA(_, fms) => fms
                .iter()
                .rev()
                .fold((Vec::new(), Vec::new()), |(pr, cr), x| {
                    let (mut pl, mut cl) = x.predconst_unique();
                    crate::union1(&mut pl, pr);
                    crate::union1(&mut cl, cr);
                    (pl, cl)
                }),
            Neg(fm) | Quant(_, _, fm) => fm.predconst_unique(),
        }
    }
}

impl<P, C, V: Clone + Eq + Hash> QNnf<Lit<P, C, V>, V> {
    pub fn fresh_vars<W>(self, map: &mut HashMap<V, W>, st: &mut W::State) -> QNnf<Lit<P, C, W>, W>
    where
        W: Clone + Fresh,
    {
        use QNnf::*;
        match self {
            Lit(lit) => Lit(lit.map_args(|tms| tms.fresh_vars(map, st))),
            BinA(o, fms) => BinA(
                o,
                fms.into_iter().map(|fm| fm.fresh_vars(map, st)).collect(),
            ),
            Quant(q, v, fm) => {
                let i = W::fresh(st);
                let old = map.insert(v.clone(), i.clone());
                let fm = fm.fresh_vars(map, st);
                match old {
                    Some(old) => map.insert(v, old),
                    None => map.remove(&v),
                };
                Quant(q, i, Box::new(fm))
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

impl<P, C: Clone + Fresh, V: Clone + Eq + Hash> QNnf<Lit<P, C, V>, V> {
    pub fn skolem_outer(self, st: &mut SkolemState<C, V>) -> Nnf<Lit<P, C, V>> {
        use QNnf::*;
        match self {
            Lit(lit) => Nnf::Lit(lit.map_args(|tms| tms.subst(&st.existential))),
            BinA(o, fms) => Nnf::BinA(o, fms.into_iter().map(|fm| fm.skolem_outer(st)).collect()),
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
        }
    }
}

impl<L> Cnf<L> {
    pub fn conjs(fms: impl DoubleEndedIterator<Item = Self>) -> Self {
        fms.rev()
            .reduce(|acc, x| x & acc)
            .unwrap_or_else(|| Self::Conj(Vec::new()))
    }
}

impl OpA {
    fn fmt_args<T: Display>(self, fms: &[T], f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fms = fms.iter();
        match (self, fms.next()) {
            (OpA::Conj, None) => write!(f, "⊤"),
            (OpA::Disj, None) => write!(f, "⊥"),
            (o, Some(fm1)) => {
                write!(f, "({}", fm1)?;
                fms.try_for_each(|fm| write!(f, " {} {}", o, fm))?;
                write!(f, ")")
            }
        }
    }
}

/// Given x and y1 o ... o yn, return x if n = 0, else x o y1 o ... o yn.
pub fn join<T>(x: T, mut ys: Vec<T>, f: impl Fn(Vec<T>) -> T) -> T {
    if ys.is_empty() {
        x
    } else {
        ys.insert(0, x);
        f(ys)
    }
}
