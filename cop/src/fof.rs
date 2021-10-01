use crate::term::{Args, Arity, Fresh, Term};
use crate::{Change, Lit};
use alloc::{boxed::Box, vec::Vec};
use core::fmt::{self, Display};
use core::hash::Hash;
use core::ops::Neg;
use hashbrown::HashMap;
use num_bigint::BigUint;

/// Full first-order formula.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Fof<A, V> {
    Atom(A),
    Neg(Box<Fof<A, V>>),
    /// binary operation
    Bin(Box<Fof<A, V>>, Op, Box<Fof<A, V>>),
    /// associative binary operation
    BinA(OpA, Vec<Fof<A, V>>),
    Quant(Quantifier, V, Box<Fof<A, V>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FofAtom<P, C, V> {
    Atom(Lit<P, C, V>),
    EqTm(Term<C, V>, Term<C, V>),
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

impl<Atom: Display, V: Display> Display for Fof<Atom, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Fof::*;
        match self {
            Atom(a) => a.fmt(f),
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

impl<P: Display, C: Display, V: Display> Display for FofAtom<P, C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Atom(a) => a.fmt(f),
            Self::EqTm(l, r) => write!(f, "{} = {}", l, r),
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

impl<A, V> Neg for Fof<A, V> {
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

impl<A, V> core::ops::BitAnd for Fof<A, V> {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Self::bina(self, OpA::Conj, rhs)
    }
}

impl<A, V> core::ops::BitOr for Fof<A, V> {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self {
        Self::bina(self, OpA::Disj, rhs)
    }
}

impl<L> core::ops::BitAnd for Nnf<L> {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self {
        Self::bina(self, OpA::Conj, rhs)
    }
}

impl<L> core::ops::BitOr for Nnf<L> {
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

impl<A, V> Fof<A, V> {
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

    pub fn forall(v: V, fm: Self) -> Self {
        Self::Quant(Quantifier::Forall, v, Box::new(fm))
    }

    pub fn foralls(vs: impl Iterator<Item = V>, fm: Self) -> Self {
        vs.fold(fm, |fm, v| Self::forall(v, fm))
    }

    pub fn add_premise(self, premise: Self) -> Self {
        match self {
            Self::Bin(a, Op::Impl, b) => Self::imp(premise & *a, *b),
            _ => Self::imp(premise, self),
        }
    }

    pub fn mark_impl(self, fm: impl Fn() -> Self) -> (Change, Self) {
        match self {
            Self::Bin(a, Op::Impl, c) => (true, Self::imp(*a & fm(), fm() & *c)),
            _ => (false, self),
        }
    }

    pub fn map_atoms<B>(self, f: &mut impl FnMut(A) -> B) -> Fof<B, V> {
        use Fof::*;
        match self {
            Atom(a) => Atom(f(a)),
            Neg(fm) => -fm.map_atoms(f),
            Bin(l, op, r) => Fof::bin(l.map_atoms(f), op, r.map_atoms(f)),
            BinA(o, fms) => BinA(o, fms.into_iter().map(|fm| fm.map_atoms(f)).collect()),
            Quant(q, v, fm) => Fof::Quant(q, v, Box::new(fm.map_atoms(f))),
        }
    }

    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> W) -> Fof<A, W> {
        use Fof::*;
        match self {
            Atom(a) => Atom(a),
            Neg(fm) => -fm.map_vars(f),
            Bin(l, o, r) => Fof::bin(l.map_vars(f), o, r.map_vars(f)),
            BinA(o, fms) => BinA(o, fms.into_iter().map(|fm| fm.map_vars(f)).collect()),
            Quant(q, v, fm) => Quant(q, f(v), Box::new(fm.map_vars(f))),
        }
    }

    pub fn atoms(&self) -> Box<dyn Iterator<Item = &A> + '_> {
        use Fof::*;
        match self {
            Atom(a) => Box::new(core::iter::once(a)),
            Neg(fm) | Quant(_, _, fm) => fm.atoms(),
            Bin(l, _, r) => Box::new(l.atoms().chain(r.atoms())),
            BinA(_, fms) => Box::new(fms.iter().flat_map(|fm| fm.atoms())),
        }
    }
}

impl<P, C, V> Fof<FofAtom<P, C, V>, V> {
    pub fn atom(p: P, args: Args<C, V>) -> Self {
        Self::Atom(FofAtom::Atom(Lit::new(p, args)))
    }

    pub fn eqtm(l: Term<C, V>, r: Term<C, V>) -> Self {
        Self::Atom(FofAtom::EqTm(l, r))
    }
}

impl<A: Clone + Neg<Output = A>, V: Clone> Fof<A, V> {
    pub fn qnnf(self) -> QNnf<A, V> {
        match self {
            // leanCoP-specific
            Self::Bin(l, Op::EqFm, r) => ((*l.clone() & *r.clone()) | (-*l & -*r)).qnnf(),
            Self::Bin(l, Op::Impl, r) => (-*l | *r).qnnf(),
            Self::Neg(x) => match *x {
                // leanCoP-specific
                Self::Bin(l, Op::EqFm, r) => ((*l.clone() & -*r.clone()) | (-*l & *r)).qnnf(),
                Self::Bin(l, Op::Impl, r) => (*l & -*r).qnnf(),
                Self::Atom(a) => QNnf::Lit(-a),
                Self::Neg(t) => t.qnnf(),
                Self::BinA(op, fms) => {
                    QNnf::BinA(-op, fms.into_iter().map(|fm| (-fm).qnnf()).collect())
                }
                Self::Quant(q, v, t) => QNnf::Quant(-q, v, Box::new((-*t).qnnf())),
            },
            Self::Atom(a) => QNnf::Lit(a),
            Self::BinA(op, fms) => QNnf::BinA(op, fms.into_iter().map(|fm| fm.qnnf()).collect()),
            Self::Quant(q, v, t) => QNnf::Quant(q, v, Box::new(t.qnnf())),
        }
    }
}

impl<P, C, V> FofAtom<P, C, V> {
    pub fn map_vars<W>(self, f: &mut impl Fn(V) -> W) -> FofAtom<P, C, W> {
        use FofAtom::*;
        let mut mv = |v| Term::V(f(v));
        match self {
            Atom(lit) => Atom(lit.map_args(|tms| tms.map_vars(&mut mv))),
            EqTm(l, r) => EqTm(l.map_vars(&mut mv), r.map_vars(&mut mv)),
        }
    }

    pub fn to_lit(self, eq: impl Fn() -> P) -> Lit<P, C, V> {
        match self {
            Self::Atom(lit) => lit,
            Self::EqTm(l, r) => Lit::new(eq(), Args::from([l, r])),
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

/*
impl<P: Clone, C: Clone, V: Clone> Form<P, C, V> {
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
}
*/

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

impl<P: Eq, C: Eq, V> Fof<FofAtom<P, C, V>, V> {
    /// Corresponds to leanCoP's `collect_predfunc`.
    pub fn predconst_unique(&self) -> (Vec<(&P, Arity)>, Vec<(&C, Arity)>) {
        use Fof::*;
        match self {
            Atom(FofAtom::Atom(a)) => (
                Vec::from([(a.head(), a.args().len())]),
                a.args().const_unique(),
            ),
            Atom(FofAtom::EqTm(l, r)) => {
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
