/// Restore the state of mutable data structures.
///
/// In the presence of backtracking, mutable data structures
/// (such as the substitution) often need to be reset to an earlier state.
/// Such data structures should implement `Rewind<T>` if
/// `T` is a cheap and small characterisation of their state.
pub trait Rewind<T> {
    /// Rewind to some state.
    fn rewind(&mut self, state: T);
}

impl<T> Rewind<usize> for alloc::vec::Vec<T> {
    fn rewind(&mut self, state: usize) {
        assert!(self.len() >= state);
        self.truncate(state)
    }
}
