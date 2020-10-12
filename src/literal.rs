use crate::term::App;
use std::fmt::{self, Display};

#[derive(Debug, Eq, PartialEq)]
pub struct Lit<C, V>(pub bool, pub App<C, V>);

impl<C: Display, V: Display> Display for Lit<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0 {
            write!(f, "{}", self.1)
        } else {
            write!(f, "¬ {}", self.1)
        }
    }
}
