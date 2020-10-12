use super::clause::Clause;
use crate::fof::Form;

#[derive(Debug)]
pub struct Matrix<C, V>(Vec<Clause<C, V>>);

impl<C, V> Matrix<C, V> {
    /// Return the conjunction of two matrices.
    fn union(mut self, mut other: Self) -> Self {
        self.0.append(&mut other.0);
        self
    }
}

impl<C: core::ops::Neg<Output = C> + Eq + Clone, V: Eq> From<Form<C, V>> for Matrix<C, V> {
    fn from(fm: Form<C, V>) -> Self {
        match fm {
            Form::Conj(l, r) => Self::from(*l).union(Self::from(*r)),
            _ => Self(vec![Clause::from(fm)]),
        }
    }
}
