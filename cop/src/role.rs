//! Formula roles and mapping from roles to formulas.

use crate::fof::{Fof, OpA, Quantifier};
use alloc::vec::Vec;

/// Formula role.
#[derive(PartialEq, Debug, Eq, Hash)]
pub enum Role {
    /// conjecture
    Conjecture,
    /// negated conjecture
    NegatedConjecture,
    /// anything else
    Other,
}

/// Associates roles with objects of type `F`, typically formulas.
#[derive(Debug, Default)]
pub struct RoleMap<F>(hashbrown::HashMap<Role, F>);

impl<F: Default> RoleMap<F> {
    /// Return a mutable reference to the object associated with the role.
    pub fn get_mut(&mut self, role: Role) -> &mut F {
        self.0.entry(role).or_default()
    }

    fn remove(&mut self, role: &Role) -> F {
        self.0.remove(role).unwrap_or_default()
    }
}

impl Role {
    /// Unquantified variables in formulas of the role are implicitly quantified with the returned quantifier.
    pub fn quantifier(&self) -> Quantifier {
        match self {
            Role::Conjecture => Quantifier::Exists,
            _ => Quantifier::Forall,
        }
    }
}

impl<A, V> RoleMap<Vec<Fof<A, V>>> {
    /// Merge all formulas to a big formula.
    ///
    /// This returns `None` only if the role map contains no formula.
    pub fn join(mut self) -> Option<Fof<A, V>> {
        let mut th = self.remove(&Role::Other);
        let mut cj = self.remove(&Role::Conjecture);
        let nc = self.remove(&Role::NegatedConjecture);
        cj.extend(nc.into_iter().map(|fm| -fm));
        let conj = |mut fms: Vec<_>, fm1| {
            if fms.is_empty() {
                fm1
            } else {
                fms.push(fm1);
                Fof::BinA(OpA::Conj, fms)
            }
        };
        match (th.pop(), cj.pop()) {
            (Some(th1), Some(cj1)) => Some(Fof::imp(conj(th, th1), conj(cj, cj1))),
            (Some(th1), None) => Some(-conj(th, th1)),
            (None, Some(cj1)) => Some(conj(cj, cj1)),
            _ => None,
        }
    }
}
