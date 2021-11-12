mod clause;
mod contrapositive;
mod matrix;
pub mod search;

use crate::Lit;

pub use contrapositive::PreCp;
pub type Db<'a, P, C, V> = crate::database::Db<P, PreCp<'a, Lit<P, C, V>, V>>;

pub use clause::Clause;
pub use matrix::Matrix;
pub use search::Search;
