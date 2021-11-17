use crate::fof::{Fof, OpA, Quantifier};
use alloc::vec::Vec;

#[derive(PartialEq, Debug, Eq, Hash)]
pub enum Role {
    Conjecture,
    NegatedConjecture,
    Other,
}

#[derive(Debug, Default)]
pub struct RoleMap<F>(hashbrown::HashMap<Role, F>);

impl<F: Default> RoleMap<F> {
    pub fn get_mut(&mut self, role: Role) -> &mut F {
        self.0.entry(role).or_default()
    }

    fn remove(&mut self, role: &Role) -> F {
        self.0.remove(role).unwrap_or_default()
    }
}

impl Role {
    pub fn quantifier(&self) -> Quantifier {
        match self {
            Role::Conjecture => Quantifier::Exists,
            _ => Quantifier::Forall,
        }
    }
}

impl<A, V> RoleMap<Vec<Fof<A, V>>> {
    pub fn join(mut self) -> Option<Fof<A, V>> {
        let mut th = self.remove(&Role::Other);
        let mut cj = self.remove(&Role::Conjecture);
        let nc = self.remove(&Role::NegatedConjecture);
        cj.append(&mut nc.into_iter().map(|fm| -fm).collect());
        let conj = |mut fms: Vec<_>, fm1| {
            if fms.is_empty() {
                fm1
            } else {
                fms.push(fm1);
                Fof::BinA(OpA::Conj, fms)
            }
        };
        match (th.pop(), cj.pop()) {
            (Some(th1), Some(gl1)) => Some(Fof::imp(conj(th, th1), conj(cj, gl1))),
            (Some(th1), None) => Some(-conj(th, th1)),
            (None, Some(gl1)) => Some(conj(cj, gl1)),
            _ => None,
        }
    }
}
