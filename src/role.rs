use crate::fof::{Form, Op};
use tptp::top;

#[derive(PartialEq, Debug, Eq, Hash)]
pub enum Role {
    Conjecture,
    NegatedConjecture,
    Other,
}

#[derive(Debug, Default)]
pub struct RoleMap<F>(std::collections::HashMap<Role, F>);

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
        let th = self.remove(&Role::Other);
        let pc = self.remove(&Role::Conjecture);
        let nc = self.remove(&Role::NegatedConjecture);
        let th = Form::bins(th, Op::Conj);
        let gl = Form::bins(pc.into_iter().chain(nc.into_iter()).collect(), Op::Conj);
        match (th, gl) {
            (Some(th), Some(gl)) => Some(Form::imp(th, gl)),
            (Some(th), None) => Some(-th),
            (None, Some(gl)) => Some(gl),
            _ => None,
        }
    }
}

impl From<top::FormulaRole> for Role {
    fn from(role: top::FormulaRole) -> Self {
        use top::FormulaRole::*;
        match role {
            Conjecture => Self::Conjecture,
            NegatedConjecture => Self::NegatedConjecture,
            _ => Self::Other,
        }
    }
}
