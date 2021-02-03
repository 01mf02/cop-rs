use crate::fof::{Form, Op};
use alloc::vec::Vec;
use tptp::top;

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

impl<P, C, V> RoleMap<Vec<Form<P, C, V>>> {
    pub fn join(mut self) -> Option<Form<P, C, V>> {
        let th = self.remove(&Role::Other).into_iter();
        let pc = self.remove(&Role::Conjecture).into_iter();
        let nc = self.remove(&Role::NegatedConjecture).into_iter();
        let th = Form::bins(th, Op::Conj);
        let gl = Form::bins(pc.chain(nc.map(|fm| -fm)), Op::Conj);
        match (th, gl) {
            (Some(th), Some(gl)) => Some(Form::imp(th, gl)),
            (Some(th), None) => Some(-th),
            (None, Some(gl)) => Some(gl),
            _ => None,
        }
    }
}

impl From<top::FormulaRole<'_>> for Role {
    fn from(role: top::FormulaRole<'_>) -> Self {
        match role.0 .0 {
            "conjecture" => Self::Conjecture,
            "negated_conjecture" => Self::NegatedConjecture,
            _ => Self::Other,
        }
    }
}
