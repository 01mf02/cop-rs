use crate::clause::OClause;
use crate::offset::OLit;
use crate::{Clause, Lit, Offset};
use core::fmt::{self, Display};

/// Clausal contrapositive for a literal `lit` with some inferred information about it.
///
/// This is usually produced by [`super::Matrix::contrapositives`].
#[derive(Debug)]
pub struct Contrapositive<'t, L, V> {
    /// the actual contrapositive, i.e. the clause containing `lit` minus `lit`
    pub contra: crate::clause::Contrapositive<&'t L>,
    /// the maximum variable of the clause
    pub vars: Option<V>,
}

impl<'t, P: Clone, C, V> Contrapositive<'t, Lit<P, C, V>, V> {
    /// Convert the contrapositive into a form that allows it to be inserted into the database.
    pub fn db_entry(self) -> (P, Self) {
        (self.contra.lit.head().clone(), self)
    }
}

/// Offset contrapositive.
pub type OContrapositive<'t, P, C> =
    Offset<&'t crate::clause::Contrapositive<&'t Lit<P, C, usize>>>;

impl<'t, L: Display, V> Display for Contrapositive<'t, L, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.contra.fmt(f)
    }
}

impl<'t, P: Display, C: Display> Display for OContrapositive<'t, P, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let rest: Clause<_> = self.rest().into_iter().map(|x| x.copied()).collect();
        write!(f, "{} ∨ {}", self.lit(), rest)
    }
}

impl<'t, P, C> OContrapositive<'t, P, C> {
    /// The literal corresponding to the contrapositive.
    pub fn lit(&self) -> OLit<'t, P, C> {
        self.map(|c| c.lit)
    }

    /// The clause minus the literal corresponding to the contrapositive.
    pub fn rest(&self) -> OClause<'t, &'t Lit<P, C, usize>> {
        self.map(|c| &c.rest)
    }
}
