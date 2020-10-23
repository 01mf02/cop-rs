use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::rc::Rc;
use tptp::syntax;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Args<C, V>(Vec<Term<C, V>>);

impl<C, V> Args<C, V> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> Args<C, W> {
        Args(self.0.into_iter().map(|tm| tm.map_vars(f)).collect())
    }

    pub fn iter(&self) -> impl Iterator<Item = &Term<C, V>> {
        self.0.iter()
    }
}

impl<C, V: Ord> Args<C, V> {
    pub fn max_var(&self) -> Option<&V> {
        use core::cmp::max;
        self.iter().fold(None, |acc, tm| max(tm.max_var(), acc))
    }
}

impl<'a, C, V> IntoIterator for &'a Args<C, V> {
    type Item = &'a Term<C, V>;
    type IntoIter = core::slice::Iter<'a, Term<C, V>>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<C: Display, V: Display> Display for Args<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.iter();
        if let Some(arg) = iter.next() {
            write!(f, "({}", arg)?;
            for arg in iter {
                write!(f, ", {}", arg)?;
            }
            write!(f, ")")?
        }
        Ok(())
    }
}

impl<C: Clone, V: Clone + Eq + Hash> Args<C, V> {
    pub fn subst(self, sub: &HashSubst<C, V>) -> Self {
        self.map_vars(&mut |v| match sub.get(&v) {
            Some(tm) => tm.clone(),
            None => Term::V(v),
        })
    }
}

impl<C, V: Eq + Hash> Args<C, V> {
    pub fn univar<W: Clone>(self, map: HashMap<V, W>) -> Args<C, W> {
        self.map_vars(&mut |v| Term::V(map.get(&v).unwrap().clone()))
    }
}

pub trait Fresh {
    type State;
    fn fresh(st: &mut Self::State) -> Self;
}

impl Fresh for String {
    type State = (String, usize);
    fn fresh(st: &mut Self::State) -> Self {
        st.1 += 1;
        format!("{}{}", st.0, st.1)
    }
}

impl Fresh for usize {
    type State = usize;
    fn fresh(st: &mut Self::State) -> Self {
        *st += 1;
        *st
    }
}

pub type Subst<C> = Vec<Option<Rc<Term<C, usize>>>>;
pub type HashSubst<C, V> = HashMap<V, Term<C, V>>;

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
        Self::C(C::fresh(st), Args(args.into_iter().map(Self::V).collect()))
    }
}

pub type STerm = Term<String, String>;

impl From<syntax::FofFunctionTerm<'_>> for STerm {
    fn from(tm: syntax::FofFunctionTerm) -> Self {
        use syntax::FofFunctionTerm::*;
        match tm {
            Plain(fpt) => Self::from(fpt),
            _ => todo!(),
        }
    }
}

impl From<syntax::FofTerm<'_>> for STerm {
    fn from(tm: syntax::FofTerm) -> Self {
        use syntax::FofTerm::*;
        match tm {
            Variable(v) => Self::V(v.to_string()),
            Function(f) => Self::from(f),
        }
    }
}

impl From<syntax::FofArguments<'_>> for SArgs {
    fn from(args: syntax::FofArguments) -> Self {
        Self(args.0.into_iter().map(Term::from).collect())
    }
}

impl From<syntax::FofPlainTerm<'_>> for STerm {
    fn from(tm: syntax::FofPlainTerm) -> Self {
        use syntax::FofPlainTerm::*;
        match tm {
            Constant(c) => Self::C(c.to_string(), Args::new()),
            Function(f, args) => Self::C(f.to_string(), Args::from(args)),
        }
    }
}
