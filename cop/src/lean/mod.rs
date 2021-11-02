mod clause;
mod context;
mod contrapositive;
mod cuts;
mod matrix;
mod proof;
pub mod search;

pub use clause::Clause;
pub use context::Context;
pub use contrapositive::Contrapositive;
pub use cuts::Cuts;
pub use matrix::Matrix;
pub use proof::Proof;
pub use search::Search;

pub type Db<P, C, V> = crate::database::Db<P, Contrapositive<P, C, V>>;
