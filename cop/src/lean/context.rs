//! Path and lemmas.
use crate::Rewind;
use alloc::vec::Vec;

/// Path and lemmas.
#[derive(Copy, Clone, Default)]
pub struct Context<T> {
    /// literals that we currently attempt to solve
    pub path: T,
    /// literals that we have already solved, assuming the path
    pub lemmas: T,
}

/// Information to rewind a context to an earlier state.
pub type Ptr = Context<usize>;

impl<T> From<&Context<Vec<T>>> for Ptr {
    fn from(ctx: &Context<Vec<T>>) -> Self {
        Self {
            path: ctx.path.len(),
            lemmas: ctx.lemmas.len(),
        }
    }
}

impl<T: Rewind<R>, R> Rewind<Context<R>> for Context<T> {
    fn rewind(&mut self, ptr: Context<R>) {
        self.path.rewind(ptr.path);
        self.lemmas.rewind(ptr.lemmas);
    }
}
