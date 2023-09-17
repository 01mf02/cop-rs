use crate::term::{Args, Fresh};
use crate::{App, Term};
use hashbrown::HashMap;

/// Literal, i.e. application of term arguments (containing constants `C` and variables `V`) to predicate `P`.
pub type Lit<P, C, V> = App<P, Args<C, V>>;

impl<P, C, V> Lit<P, C, V> {
    /// Returns true iff no variable occurs in the literal.
    pub fn is_ground(&self) -> bool {
        self.vars().next().is_none()
    }

    /// The variables occurring in the literal.
    pub fn vars(&self) -> impl Iterator<Item = &V> {
        self.args().iter().flat_map(|a| a.vars())
    }

    /// Apply a function to all variables.
    pub fn map_vars<W>(self, f: &mut impl FnMut(V) -> Term<C, W>) -> Lit<P, C, W> {
        self.map_args(|args| args.map_vars(f))
    }
}

impl<P, C, V: Eq + core::hash::Hash> Lit<P, C, V> {
    /// For every variable `v` in the literal,
    /// replace `v` by its corresponding mapping if one exists, otherwise
    /// replace `v` by a fresh variable and add a new mapping from `v` to it.
    pub fn fresh_vars<W>(self, map: &mut HashMap<V, W>, st: &mut W::State) -> Lit<P, C, W>
    where
        W: Clone + Fresh,
    {
        self.map_vars(&mut |v| Term::V(map.entry(v).or_insert_with(|| W::fresh(st)).clone()))
    }
}
