use crate::fof::Form;

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

impl<C, V> RoleMap<Vec<Form<C, V>>> {
    pub fn join(mut self) -> Option<Form<C, V>> {
        let th = self.remove(&Role::Other);
        let pc = self.remove(&Role::Conjecture);
        let nc = self.remove(&Role::NegatedConjecture);
        let th = Form::conjoin_right(th);
        let gl = Form::conjoin_right(pc.into_iter().chain(nc.into_iter()).collect());
        match (th, gl) {
            (Some(th), Some(gl)) => Some(Form::Impl(Box::new(th), Box::new(gl))),
            (Some(th), None) => Some(Form::Neg(Box::new(th))),
            (None, Some(gl)) => Some(gl),
            _ => None,
        }
    }
}

impl From<tptp::meta::FormulaRole> for Role {
    fn from(role: tptp::meta::FormulaRole) -> Self {
        use tptp::meta::FormulaRole::*;
        match role {
            Conjecture => Self::Conjecture,
            NegatedConjecture => Self::NegatedConjecture,
            _ => Self::Other,
        }
    }
}
