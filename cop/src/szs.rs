//! SZS ontologies.

use core::fmt::{self, Debug, Display};
pub use NoSuccessKind::*;
pub use SuccessKind::*;

/// SZS status.
pub struct Status<K>(pub K);

impl<K: Debug> Display for Status<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "% SZS status {:?}", self.0)
    }
}

/// SZS output.
pub struct Output<O>(pub O);

impl<O: Display> Display for Output<O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "% SZS output start")?;
        writeln!(f, "{}", self.0)?;
        writeln!(f, "% SZS output end")
    }
}

/// Success.
#[derive(Debug)]
pub enum SuccessKind {
    /// All models of Ax are models of C
    Theorem,
    /// Some interpretations are models of Ax, and
    /// some models of Ax are models of C
    Satisfiable,
}

/// Lack of success.
#[derive(Debug)]
pub enum NoSuccessKind {
    /// Software stopped due to an operating system error
    OsError,
    /// Software stopped due to an input error
    InputError,
    /// Software stopped due to an input syntax error
    SyntaxError,
    /// Software stopped due to an input semantic error
    SemanticError,
    /// Software gave up because it's incomplete
    Incomplete,
    /// Software gave up because it cannot process this type of data
    Inappropriate,
}
