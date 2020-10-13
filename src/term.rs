use core::ops::Neg;
use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::Hash;
use tptp::syntax;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct App<C, V> {
    pub c: C,
    pub args: Vec<Term<C, V>>,
}

impl<C, V> App<C, V> {
    pub fn new(c: C, args: Vec<Term<C, V>>) -> Self {
        Self { c, args }
    }

    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> App<C, W> {
        App {
            c: self.c,
            args: self.args.into_iter().map(|tm| tm.map_vars(f)).collect(),
        }
    }
}

impl<C: Neg<Output = C>, V> Neg for App<C, V> {
    type Output = Self;
    fn neg(self) -> Self {
        App {
            c: self.c.neg(),
            args: self.args,
        }
    }
}

impl<C: Display, V: Display> Display for App<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.c)?;
        let mut iter = self.args.iter();
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

impl<C: Clone, V: Clone + Eq + Hash> App<C, V> {
    pub fn subst(self, sub: &Subst<C, V>) -> Self {
        self.map_vars(&mut |v| match sub.get(&v) {
            Some(tm) => tm.clone(),
            None => Term::V(v),
        })
    }
}

impl<C, V: Eq + Hash> App<C, V> {
    pub fn univar<W: Clone>(self, map: HashMap<V, W>) -> App<C, W> {
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

pub type Subst<C, V> = HashMap<V, Term<C, V>>;

pub type SApp = App<String, String>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Term<C, V> {
    C(App<C, V>),
    V(V),
}

impl<C, V> Term<C, V> {
    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> Term<C, W> {
        match self {
            Self::C(app) => Term::C(app.map_vars(f)),
            Self::V(v) => f(v),
        }
    }
}

impl<C: Display, V: Display> Display for Term<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Term::*;
        match self {
            C(app) => app.fmt(f),
            V(v) => v.fmt(f),
        }
    }
}

impl<C: Fresh, V> Term<C, V> {
    pub fn skolem(st: &mut C::State, args: Vec<V>) -> Self {
        let c = C::fresh(st);
        let args = args.into_iter().map(Term::V).collect();
        Term::C(App::new(c, args))
    }
}

pub type STerm = Term<String, String>;

impl From<syntax::FofFunctionTerm<'_>> for STerm {
    fn from(tm: syntax::FofFunctionTerm) -> Self {
        use syntax::FofFunctionTerm::*;
        match tm {
            Plain(fpt) => Self::C(App::from(fpt)),
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

impl From<syntax::FofPlainTerm<'_>> for SApp {
    fn from(tm: syntax::FofPlainTerm) -> Self {
        use syntax::FofPlainTerm::*;
        match tm {
            Constant(c) => Self {
                c: c.to_string(),
                args: Vec::new(),
            },
            Function(f, args) => Self {
                c: f.to_string(),
                args: args.0.into_iter().map(Term::from).collect(),
            },
        }
    }
}
