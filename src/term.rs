use core::fmt::{self, Display};
use core::hash::Hash;
use std::collections::HashMap;
use tptp::fof;

pub type Arity = usize;

pub type Args<C, V> = crate::Args<Term<C, V>>;

impl<C, V> Args<C, V> {
    pub fn new() -> Self {
        core::iter::empty().collect()
    }

    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> Args<C, W> {
        self.into_iter().map(|tm| tm.map_vars(f)).collect()
    }

    pub fn constants(&self) -> impl Iterator<Item = (&C, Arity)> {
        self.into_iter().flat_map(|arg| arg.constants())
    }
}

impl<C, V: Ord> Args<C, V> {
    pub fn max_var(&self) -> Option<&V> {
        self.into_iter().map(|tm| tm.max_var()).max().flatten()
    }
}

impl<C: Clone, V: Clone + Eq + Hash> Args<C, V> {
    pub fn subst(self, sub: &HashMap<V, Term<C, V>>) -> Self {
        self.into_iter().map(|tm| tm.subst(sub)).collect()
    }
}

impl<C, V: Eq + Hash> Args<C, V> {
    pub fn fresh_vars<W>(self, map: &mut HashMap<V, W>, st: &mut W::State) -> Args<C, W>
    where
        W: Clone + Fresh,
    {
        self.into_iter().map(|tm| tm.fresh_vars(map, st)).collect()
    }
}

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

pub type SArgs = Args<String, String>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term<C, V> {
    C(C, Args<C, V>),
    V(V),
}

impl<C, V> Term<C, V> {
    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> Term<C, W> {
        match self {
            Self::C(c, args) => Term::C(c, args.map_vars(f)),
            Self::V(v) => f(v),
        }
    }

    pub fn constants(&self) -> Box<dyn Iterator<Item = (&C, Arity)> + '_> {
        match self {
            Self::C(c, args) => Box::new(core::iter::once((c, args.len())).chain(args.constants())),
            Self::V(_) => Box::new(core::iter::empty()),
        }
    }
}

impl<C, V: Ord> Term<C, V> {
    pub fn max_var(&self) -> Option<&V> {
        use Term::*;
        match self {
            C(_, args) => args.max_var(),
            V(v) => Some(v),
        }
    }
}

impl<C: Clone, V: Clone + Eq + Hash> Term<C, V> {
    pub fn subst(self, sub: &HashMap<V, Term<C, V>>) -> Self {
        self.map_vars(&mut |v| match sub.get(&v) {
            Some(tm) => tm.clone(),
            None => Term::V(v),
        })
    }
}

impl<C, V: Eq + Hash> Term<C, V> {
    pub fn fresh_vars<W>(self, map: &mut HashMap<V, W>, st: &mut W::State) -> Term<C, W>
    where
        W: Clone + Fresh,
    {
        self.map_vars(&mut |v| Term::V(map.entry(v).or_insert_with(|| W::fresh(st)).clone()))
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
    pub fn skolem(st: &mut C::State, args: Vec<V>) -> Self {
        Self::C(C::fresh(st), args.into_iter().map(Self::V).collect())
    }
}

pub type STerm = Term<String, String>;

impl From<fof::FunctionTerm<'_>> for STerm {
    fn from(tm: fof::FunctionTerm) -> Self {
        use fof::FunctionTerm::*;
        match tm {
            Plain(fpt) => Self::from(fpt),
            _ => todo!(),
        }
    }
}

impl From<fof::Term<'_>> for STerm {
    fn from(tm: fof::Term) -> Self {
        use fof::Term::*;
        match tm {
            Variable(v) => Self::V(v.to_string()),
            Function(f) => Self::from(*f),
        }
    }
}

impl From<fof::Arguments<'_>> for SArgs {
    fn from(args: fof::Arguments) -> Self {
        args.0.into_iter().map(Term::from).collect()
    }
}

impl From<fof::PlainTerm<'_>> for STerm {
    fn from(tm: fof::PlainTerm) -> Self {
        use fof::PlainTerm::*;
        match tm {
            Constant(c) => Self::C(c.to_string(), Args::new()),
            Function(f, args) => Self::C(f.to_string(), Args::from(*args)),
        }
    }
}
