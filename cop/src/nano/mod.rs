mod clause;
mod contrapositive;
mod matrix;

use crate::Lit;

pub use contrapositive::VContrapositive;
pub type Db<'a, P, C, V> = crate::database::Db<P, VContrapositive<'a, Lit<P, C, V>, V>>;

pub use clause::Clause;
pub use matrix::Matrix;
