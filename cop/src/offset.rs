//! Offset objects.
use crate::term::Args;
use crate::{Lit, LitMat, Subst, Term};
use core::fmt::{self, Display};
use core::iter;
use core::ops::Neg;
use log::trace;

/// An object with an `usize` offset for variables.
///
/// For example, when we want to create a fresh clause copy,
/// instead of creating a new clause by applying an offset to every variable (taking linear time),
/// we simply wrap a clause pointer with an `Offset` (taking constant time).
/// This approach (wrapping objects in an `Offset`) also avoids
/// mixing up offsets for different objects or
/// forgetting to consider an offset.
///
/// A principal design idea of this type is that we should not be able to
/// obtain the original, non-offset object once it is offset.
/// This is to prevent that the offset is "forgotten".
#[derive(Copy, Clone, Debug)]
pub struct Offset<T> {
    o: usize,
    x: T,
}

impl<T> Offset<&T> {
    /// Return true if the offsets and the pointers are equal.
    pub fn ptr_eq(self, other: Self) -> bool {
        core::ptr::eq(self.x, other.x) && self.o == other.o
    }
}

impl<T: Copy> Offset<&T> {
    /// Copy the contained object.
    pub fn copied(self) -> Offset<T> {
        self.map(|x| *x)
    }
}

impl<T> Offset<T> {
    /// Create a new offset object.
    pub fn new(o: usize, x: T) -> Offset<T> {
        Offset { o, x }
    }

    /// Apply a function to the contained object.
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Offset<U> {
        let x = f(self.x);
        Offset { o: self.o, x }
    }

    /// Replace the offset item with another.
    fn put<U>(&self, x: U) -> Offset<U> {
        Offset { o: self.o, x }
    }
}

type ZipWith<T, I> = iter::Zip<iter::Repeat<T>, <I as iter::IntoIterator>::IntoIter>;
type OffsetFn<T> = fn((usize, T)) -> Offset<T>;

/// The type signature of [`Offset::copied`].
pub type CopiedFn<T> = fn(Offset<&T>) -> Offset<T>;

/// Convert an offset of a collection of `T`s to a collection of offset `T`s.
impl<T, I: IntoIterator<Item = T>> IntoIterator for Offset<I> {
    type Item = Offset<T>;
    type IntoIter = iter::Map<ZipWith<usize, I>, OffsetFn<T>>;
    fn into_iter(self) -> Self::IntoIter {
        let zipped = iter::repeat(self.o).zip(self.x);
        zipped.map(|(o, x)| Offset { o, x })
    }
}

/// Offset literal.
pub type OLit<'t, P, C> = Offset<&'t Lit<P, C, usize>>;
/// Offset term.
pub type OTerm<'t, C> = Offset<&'t Term<C, usize>>;
/// Offset arguments.
pub type OArgs<'t, C> = Offset<&'t Args<C, usize>>;

/// Offset litmat.
pub type OLitMat<'t, L, M> = Offset<&'t LitMat<L, M>>;

/// Substitution for offset terms.
pub type Sub<'t, C> = Subst<OTerm<'t, C>>;

impl<'t, C> OTerm<'t, C> {
    /// Return true if the substituted term contains the given variable.
    fn contains_mod(self, sub: &Sub<'t, C>, v: usize) -> bool {
        // TODO: implement `checked` optimisation if bottleneck
        use Term::*;
        match self.x {
            C(_, args) => self.put(args).contains_mod(sub, v),
            V(w) => {
                w + self.o == v
                    || match sub.get(w + self.o) {
                        None => false,
                        Some(ot) => ot.contains_mod(sub, v),
                    }
            }
        }
    }

    /// Substitute the head of the term until a fix point is reached.
    ///
    /// ~~~
    /// # use cop::Term;
    /// # use cop::offset::{Sub, OTerm};
    /// let mut sub: Sub<&str> = Sub::default();
    /// sub.set_dom_max(3);
    ///
    /// let tm0: Term<&str, _> = Term::V(0);
    /// let tm1: Term<&str, _> = Term::V(1);
    /// let ot0 = OTerm::new(2, &tm0);
    /// let ot1 = OTerm::new(0, &tm1);
    /// sub.insert(2, ot1);
    ///
    /// // 0+2 should be substituted to 1+0 and terminate there
    /// assert!(ot0.whnf(&sub).ptr_eq(ot1))
    /// ~~~
    pub fn whnf(self, sub: &Sub<'t, C>) -> Self {
        sub.fix(self, |tm| match tm.x {
            Term::V(v) => Some(v + tm.o),
            _ => None,
        })
    }
}

impl<'t, C: Eq> OTerm<'t, C> {
    /// Return true if the term is equal to another term, modulo substitution.
    fn eq_mod(self, sub: &Sub<'t, C>, other: Self) -> bool {
        let l = self.whnf(sub);
        let r = other.whnf(sub);
        use Term::*;
        match (l.x, r.x) {
            (V(vl), V(vr)) => vl + l.o == vr + r.o,
            (C(cl, al), C(cr, ar)) => cl == cr && l.put(al).eq_mod(sub, r.put(ar)),
            _ => false,
        }
    }

    /// Return true if two terms can be unified, updating the substitution.
    ///
    /// This leaves the substitution in an inconsistent state when returning false!
    fn unify(self, sub: &mut Sub<'t, C>, other: Self) -> bool {
        trace!("term unify");
        let l = self.whnf(sub);
        let r = other.whnf(sub);
        if l.ptr_eq(r) {
            return true;
        };
        use Term::*;
        match (l.x, r.x, l.o, r.o) {
            (V(vl), V(vr), lo, ro) => {
                if vl + lo != vr + ro {
                    sub.insert(vl + lo, r)
                };
                true
            }
            (V(v), c @ C(_, _), vo, ao) | (c @ C(_, _), V(v), ao, vo) => {
                let c = Offset { o: ao, x: c };
                if c.contains_mod(sub, v + vo) {
                    false
                } else {
                    sub.insert(v + vo, c);
                    true
                }
            }
            (C(cl, al), C(cr, ar), _, _) => cl == cr && l.put(al).unify(sub, r.put(ar)),
        }
    }
}

impl<'t, C> OArgs<'t, C> {
    fn contains_mod(self, sub: &Sub<'t, C>, v: usize) -> bool {
        self.into_iter().any(|x| x.contains_mod(sub, v))
    }
}

impl<'t, C: Eq> OArgs<'t, C> {
    /// Return true if two offset argument sequences are equal modulo substitution.
    pub fn eq_mod(self, sub: &Sub<'t, C>, other: Self) -> bool {
        self.into_iter()
            .zip(other)
            .all(|(ot1, ot2)| ot1.eq_mod(sub, ot2))
    }

    /// Return true if two offset argument sequences can be unified under the substitution,
    /// updating the substitution.
    pub fn unify(self, sub: &mut Sub<'t, C>, other: Self) -> bool {
        self.into_iter()
            .zip(other)
            .all(|(ot1, ot2)| ot1.unify(sub, ot2))
    }
}

impl<'t, P, C> OLit<'t, P, C> {
    /// Head of the literal.
    pub fn head(&self) -> &P {
        self.x.head()
    }

    /// Offset arguments of the literal.
    pub fn args(&self) -> OArgs<'t, C> {
        self.map(|l| l.args())
    }
}

impl<'t, P: Eq, C: Eq> OLit<'t, P, C> {
    /// Return true if two offset literals are equal modulo substitution.
    pub fn eq_mod(&self, sub: &Sub<'t, C>, other: &Self) -> bool {
        self.head() == other.head() && self.args().eq_mod(sub, other.args())
    }
}

impl<'t, P: Eq + Neg<Output = P> + Clone, C: Eq> OLit<'t, P, C> {
    /// Return true if the negation of an offset literals equals another offset literal, modulo substitution.
    pub fn neg_eq_mod(&self, sub: &Sub<'t, C>, other: &Self) -> bool {
        &-self.head().clone() == other.head() && self.args().eq_mod(sub, other.args())
    }
}

impl<'t, L, M> OLitMat<'t, L, M> {
    /// Propagate the offset of a litmat to its contained literal/matrix.
    pub fn transpose(self) -> LitMat<Offset<&'t L>, Offset<&'t M>> {
        match self.x {
            LitMat::Lit(l) => LitMat::Lit(Offset { x: l, o: self.o }),
            LitMat::Mat(m) => LitMat::Mat(Offset { x: m, o: self.o }),
        }
    }
}

impl<'t, P: Display, C: Display> Display for OLit<'t, P, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.head(), self.args())
    }
}

impl<'t, C: Display> Display for OArgs<'t, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.into_iter().collect::<crate::Args<_>>().fmt(f)
    }
}

impl<'t, C: Display> Display for OTerm<'t, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.x {
            Term::C(c, args) => write!(f, "{}{}", c, self.put(args)),
            Term::V(v) => write!(f, "{}", v + self.o),
        }
    }
}
