use crate::term::{Args, Fresh};
use crate::{App, Term};
use hashbrown::HashMap;

pub type Lit<P, C, V> = App<P, Args<C, V>>;

impl<P, C, V> Lit<P, C, V> {
    pub fn is_ground(&self) -> bool {
        self.vars().next().is_none()
    }

    pub fn vars(&self) -> impl Iterator<Item = &V> {
        self.args().iter().flat_map(|a| a.vars())
    }

    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> Lit<P, C, W> {
        self.map_args(|args| args.map_vars(f))
    }
}

impl<P, C, V: Eq + core::hash::Hash> Lit<P, C, V> {
    pub fn fresh_vars<W>(self, map: &mut HashMap<V, W>, st: &mut W::State) -> Lit<P, C, W>
    where
        W: Clone + Fresh,
    {
        self.map_vars(&mut |v| Term::V(map.entry(v).or_insert_with(|| W::fresh(st)).clone()))
    }
}
