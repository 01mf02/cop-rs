use super::clause::{Clause, VClause};
use super::Matrix;
use crate::{Lit, LitMat, Signed};

impl<P: Clone, C: Clone, V: Clone> Matrix<Lit<Signed<P>, C, V>, V> {
    /// A matrix is positive if some of its elements are positive.
    pub fn positive(&self) -> Self {
        self.iter().filter_map(|cl| cl.positive()).collect()
    }
}

impl<P: Clone, C: Clone, V: Clone> Clause<Lit<Signed<P>, C, V>, Matrix<Lit<Signed<P>, C, V>, V>> {
    /// A clause is positive if all its elements are positive.
    pub fn positive(&self) -> Option<Self> {
        self.iter().map(|lm| lm.positive()).collect()
    }
}

impl<P: Clone, C: Clone, V: Clone> VClause<Lit<Signed<P>, C, V>, V> {
    pub fn positive(&self) -> Option<Self> {
        self.1.positive().map(|cl| Self(self.0.clone(), cl))
    }
}

impl<P: Clone, C: Clone, V: Clone> LitMat<Lit<Signed<P>, C, V>, Matrix<Lit<Signed<P>, C, V>, V>> {
    pub fn positive(&self) -> Option<Self> {
        match self {
            LitMat::Lit(l) => l.head().is_sign_negative().then(|| LitMat::Lit(l.clone())),
            LitMat::Mat(m) => Some(m.positive())
                .filter(|m| !m.is_empty())
                .map(LitMat::Mat),
        }
    }
}
