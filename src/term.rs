use std::collections::HashMap;
use std::fmt::{self, Display};
use std::hash::Hash;
use std::rc::Rc;
use tptp::syntax;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct App<C, V> {
    pub c: C,
    pub args: Args<C, V>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Args<C, V>(Vec<Term<C, V>>);

impl<C, V> Args<C, V> {
    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> Args<C, W> {
        Args(self.0.into_iter().map(|tm| tm.map_vars(f)).collect())
    }
}

impl<C, V: Ord> Args<C, V> {
    pub fn max_var(&self) -> Option<&V> {
        use core::cmp::max;
        self.0.iter().fold(None, |acc, tm| max(tm.max_var(), acc))
    }
}

impl<C: Eq> Args<C, usize> {
    pub fn eq_mod(&self, sub: &Subst<C>, other: &Self) -> bool {
        self.0
            .iter()
            .zip(other.0.iter())
            .all(|(t1, t2)| t1.eq_mod(sub, t2))
    }
}

impl<C: Display, V: Display> Display for Args<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.0.iter();
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

impl<C, V> App<C, V> {
    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> App<C, W> {
        App {
            c: self.c,
            args: self.args.map_vars(f),
        }
    }
}

impl<C, V: Ord> App<C, V> {
    pub fn max_var(&self) -> Option<&V> {
        self.args.max_var()
    }
}

impl<C: Display, V: Display> Display for App<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.c, self.args)
    }
}

impl<C: Clone, V: Clone + Eq + Hash> App<C, V> {
    pub fn subst(self, sub: &HashSubst<C, V>) -> Self {
        self.map_vars(&mut |v| match sub.get(&v) {
            Some(tm) => tm.clone(),
            None => Term::V(v),
        })
    }
}

impl<C: Eq> Term<C, usize> {
    pub fn eq_mod(&self, sub: &Subst<C>, other: &Self) -> bool {
        use Term::*;
        match (self, other) {
            (C(l), C(r)) => l.c == r.c && l.args.eq_mod(sub, &r.args),
            (tm, V(v)) => match &sub[*v] {
                Some(vtm) => tm.eq_mod(sub, &vtm),
                None => tm.eq_mod_var(sub, *v),
            },
            (V(v), tm) => match &sub[*v] {
                Some(vtm) => vtm.eq_mod(sub, &tm),
                None => tm.eq_mod_var(sub, *v),
            },
        }
    }

    pub fn eq_mod_var(&self, sub: &Subst<C>, other: usize) -> bool {
        use Term::*;
        match self {
            C(_) => false,
            V(v) if *v == other => true,
            V(v) => match &sub[*v] {
                None => false,
                Some(vtm) => vtm.eq_mod_var(sub, other),
            },
        }
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

pub type Subst<C> = Vec<Option<Rc<Term<C, usize>>>>;
pub type HashSubst<C, V> = HashMap<V, Term<C, V>>;

pub type SApp = App<String, String>;
pub type SArgs = Args<String, String>;

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

impl<C, V: Ord> Term<C, V> {
    pub fn max_var(&self) -> Option<&V> {
        use Term::*;
        match self {
            C(app) => app.max_var(),
            V(v) => Some(v),
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
        let args = Args(args.into_iter().map(Term::V).collect());
        Term::C(App { c, args })
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

impl From<syntax::FofArguments<'_>> for SArgs {
    fn from(args: syntax::FofArguments) -> Self {
        Self(args.0.into_iter().map(Term::from).collect())
    }
}

impl From<syntax::FofPlainTerm<'_>> for SApp {
    fn from(tm: syntax::FofPlainTerm) -> Self {
        use syntax::FofPlainTerm::*;
        match tm {
            Constant(c) => Self {
                c: c.to_string(),
                args: Args(Vec::new()),
            },
            Function(f, args) => Self {
                c: f.to_string(),
                args: Args::from(args),
            },
        }
    }
}
