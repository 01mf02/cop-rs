use core::fmt::{self, Debug, Display};
pub use NoSuccessKind::*;
pub use SuccessKind::*;

pub struct Status<K>(pub K);

impl<K: Debug> Display for Status<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "% SZS status {:?}", self.0)
    }
}

pub struct Output<O>(pub O);

impl<O: Display> Display for Output<O> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "% SZS output start")?;
        writeln!(f, "{}", self.0)?;
        writeln!(f, "% SZS output end")
    }
}

#[derive(Debug)]
pub enum SuccessKind {
    Theorem,
    Satisfiable,
}

#[derive(Debug)]
pub enum NoSuccessKind {
    OsError,
    InputError,
    SyntaxError,
    SemanticError,
    Incomplete,
    Inappropriate,
}
