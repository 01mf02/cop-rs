mod clause;
mod context;
mod contrapositive;
mod cuts;
mod matrix;
mod proof;
pub mod search;

pub use context::Context;
pub use contrapositive::Contrapositive;
pub use cuts::Cuts;
pub use matrix::Matrix;
pub use proof::Proof;
pub use search::Search;

pub type Db<'t, P, C, V> = crate::database::Db<P, Contrapositive<'t, crate::Lit<P, C, V>, V>>;
