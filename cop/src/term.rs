use alloc::{boxed::Box, format, string::String, vec::Vec};
use core::fmt::{self, Display};
use core::hash::Hash;
use hashbrown::HashMap;

/// Number of arguments of a constant.
pub type Arity = usize;

/// Term arguments.
pub type Args<C, V> = crate::Args<Term<C, V>>;

impl<C, V> Args<C, V> {
    /// Create empty arguments.
    pub fn new() -> Self {
        Self::from([])
    }

    /// Apply function to the constants of the arguments.
    pub fn map_constants<D>(self, f: &mut impl FnMut(C) -> D) -> Args<D, V> {
        self.into_iter().map(|tm| tm.map_constants(f)).collect()
    }

    /// Apply function to the variables of the arguments.
    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> Args<C, W> {
        self.into_iter().map(|tm| tm.map_vars(f)).collect()
    }

    /// Return all constants occurring in the arguments.
    pub fn constants(&self) -> impl Iterator<Item = (&C, Arity)> {
        self.into_iter().flat_map(|arg| arg.constants())
    }

    /// Return all variables occurring in the arguments.
    pub fn vars(&self) -> impl Iterator<Item = &V> {
        self.into_iter().flat_map(|arg| arg.vars())
    }
}

impl<C: Clone, V: Clone + Eq + Hash> Args<C, V> {
    /// Apply a substitution to the arguments.
    pub fn subst(self, sub: &HashMap<V, Term<C, V>>) -> Self {
        self.into_iter().map(|tm| tm.subst(sub)).collect()
    }
}

impl<C: Eq, V> Args<C, V> {
    /// Corresponds to leanCoP's `collect_func`.
    pub fn const_unique(&self) -> Vec<(&C, Arity)> {
        self.iter().rev().fold(Vec::new(), |acc, x| {
            let mut cs = x.const_unique();
            crate::union1(&mut cs, acc);
            cs
        })
    }
}

/// A fresh symbol generator.
pub trait Fresh {
    type State;
    fn fresh(st: &mut Self::State) -> Self;
}

impl Fresh for String {
    type State = (String, usize);
    fn fresh(st: &mut Self::State) -> Self {
        let fresh = format!("{}{}", st.0, st.1);
        st.1 += 1;
        fresh
    }
}

impl Fresh for usize {
    type State = usize;
    fn fresh(st: &mut Self::State) -> Self {
        let fresh = *st;
        *st += 1;
        fresh
    }
}

/// First-order logic term.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term<C, V> {
    /// Constant with arguments applied to it
    C(C, Args<C, V>),
    /// Variable
    V(V),
}

impl<C, V> Term<C, V> {
    /// Apply function to the constants of the term.
    pub fn map_constants<D>(self, f: &mut impl FnMut(C) -> D) -> Term<D, V> {
        match self {
            Self::C(c, args) => Term::C(f(c), args.map_constants(f)),
            Self::V(v) => Term::V(v),
        }
    }

    /// Apply function to the variables of the term.
    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> Term<C, W> {
        match self {
            Self::C(c, args) => Term::C(c, args.map_vars(f)),
            Self::V(v) => f(v),
        }
    }

    /// Return all constants occurring in the term.
    pub fn constants(&self) -> Box<dyn Iterator<Item = (&C, Arity)> + '_> {
        match self {
            Self::C(c, args) => Box::new(core::iter::once((c, args.len())).chain(args.constants())),
            Self::V(_) => Box::new(core::iter::empty()),
        }
    }

    /// Return all variables occurring in the term.
    pub fn vars(&self) -> Box<dyn Iterator<Item = &V> + '_> {
        match self {
            Self::C(_, args) => Box::new(args.vars()),
            Self::V(v) => Box::new(core::iter::once(v)),
        }
    }
}

impl<C: Clone, V: Clone + Eq + Hash> Term<C, V> {
    /// Apply a substitution to the term.
    pub fn subst(self, sub: &HashMap<V, Term<C, V>>) -> Self {
        self.map_vars(&mut |v| match sub.get(&v) {
            Some(tm) => tm.clone(),
            None => Term::V(v),
        })
    }
}

impl<C: Eq, V> Term<C, V> {
    /// Corresponds to leanCoP's `collect_func`.
    pub fn const_unique(&self) -> Vec<(&C, Arity)> {
        match self {
            Term::V(_) => Vec::new(),
            Term::C(c, args) => {
                let mut cs = Vec::from([(c, args.len())]);
                crate::union1(&mut cs, args.const_unique());
                cs
            }
        }
    }
}

impl<C: Display, V: Display> Display for Term<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Term::*;
        match self {
            C(c, args) => write!(f, "{}{}", c, args),
            V(v) => v.fmt(f),
        }
    }
}

impl<C: Fresh, V> Term<C, V> {
    /// Return a fresh Skolem function and apply a sequence of variables.
    pub fn skolem(st: &mut C::State, args: Vec<V>) -> Self {
        Self::C(C::fresh(st), args.into_iter().map(Self::V).collect())
    }
}
