use crate::term::{App, Args};
use crate::{Lit, Term};

#[derive(Copy, Clone)]
pub struct Offset<T> {
    o: usize,
    x: T,
}

impl<T> Offset<&T> {
    /// Return true if the offsets and the pointers are equal.
    fn ptr_eq(self, other: Self) -> bool {
        core::ptr::eq(self.x, other.x) && self.o == other.o
    }
}

impl<T> Offset<T> {
    /// Replace the offset item with another.
    fn put<U>(self, x: U) -> Offset<U> {
        Offset { o: self.o, x }
    }
}

impl<T, I: IntoIterator<Item = T>> Offset<I> {
    /// Convert an offset of a collection of `T`s to a collection of offset `T`s.
    fn iter(self) -> impl Iterator<Item = Offset<T>> {
        let o = self.o;
        self.x.into_iter().map(move |x| Offset { o, x })
    }
}

pub type OLit<'t, C> = Offset<&'t Lit<C, usize>>;
pub type OTerm<'t, C> = Offset<&'t Term<C, usize>>;
pub type OArgs<'t, C> = Offset<&'t Args<C, usize>>;

pub type Sub<'t, C> = crate::subst::Subst<OTerm<'t, C>>;

impl<'t, C> OTerm<'t, C> {
    /// Return true if the substituted term contains the given variable.
    fn contains_mod(self, sub: &Sub<'t, C>, v: usize) -> bool {
        // TODO: implement `checked` optimisation if bottleneck
        use Term::*;
        match self.x {
            C(app) => self.put(&app.args).contains_mod(sub, v),
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
    fn whnf(self, sub: &Sub<'t, C>) -> Self {
        match self.x {
            Term::V(v) => match sub.get(v + self.o) {
                Some(tm) => tm.whnf(sub),
                None => self,
            },
            tm => self,
        }
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
            (C(al), C(ar)) => al.c == ar.c && l.put(&al.args).eq_mod(sub, r.put(&ar.args)),
            _ => false,
        }
    }

    /// Return true if two terms can be unified, updating the substitution.
    ///
    /// This leaves the substitution in an inconsistent state when returning false!
    fn unify(self, sub: &mut Sub<'t, C>, other: Self) -> bool {
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
            (V(v), C(a), vo, ao) | (C(a), V(v), ao, vo) => {
                let args = Offset { o: ao, x: &a.args };
                if args.contains_mod(sub, v + vo) {
                    false
                } else {
                    sub.insert(v + vo, r);
                    true
                }
            }
            (C(al), C(ar), lo, ro) => al.c == ar.c && l.put(&al.args).unify(sub, r.put(&ar.args)),
        }
    }
}

impl<'t, C> OArgs<'t, C> {
    fn contains_mod(self, sub: &Sub<'t, C>, v: usize) -> bool {
        self.iter().any(|x| x.contains_mod(sub, v))
    }
}

impl<'t, C: Eq> OArgs<'t, C> {
    fn eq_mod(self, sub: &Sub<'t, C>, other: Self) -> bool {
        self.iter()
            .zip(other.iter())
            .all(|(ot1, ot2)| ot1.eq_mod(sub, ot2))
    }

    fn unify(self, sub: &mut Sub<'t, C>, other: Self) -> bool {
        self.iter()
            .zip(other.iter())
            .all(|(ot1, ot2)| ot1.unify(sub, ot2))
    }
}

impl<'t, C> OLit<'t, C> {
    fn args(self) -> OArgs<'t, C> {
        self.put(self.x.args())
    }
}

impl<'t, C: Eq> OLit<'t, C> {
    fn eq_mod(self, sub: &Sub<'t, C>, other: Self) -> bool {
        self.x.head() == other.x.head() && self.args().eq_mod(sub, other.args())
    }

    fn unify(self, sub: &mut Sub<'t, C>, other: Self) -> bool {
        self.x.head() == other.x.head() && self.args().unify(sub, other.args())
    }
}
