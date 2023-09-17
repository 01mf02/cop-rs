//! Nonclausal proof search à la nanoCoP.

pub mod clause;
mod contrapositive;
mod matrix;
mod positive;
mod proof;
pub mod search;

use crate::Lit;

pub use contrapositive::PreCp;
/// Nonclausal database.
pub type Db<'a, P, C, V> = crate::database::Db<P, PreCp<'a, Lit<P, C, V>, V>>;

pub use clause::Clause;
pub use matrix::Matrix;
pub use proof::Proof;
pub use search::Search;
