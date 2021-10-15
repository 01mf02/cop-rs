use crate::Rewind;
use alloc::vec::Vec;

#[derive(Copy, Clone, Default)]
pub struct Context<T> {
    pub path: T,
    pub lemmas: T,
}

pub type Ptr = Context<usize>;

impl<'t, T> From<&Context<Vec<T>>> for Ptr {
    fn from(ctx: &Context<Vec<T>>) -> Self {
        Self {
            path: ctx.path.len(),
            lemmas: ctx.lemmas.len(),
        }
    }
}

impl<T> Rewind<Ptr> for Context<Vec<T>> {
    fn rewind(&mut self, ptr: Ptr) {
        assert!(self.path.len() >= ptr.path);
        assert!(self.lemmas.len() >= ptr.lemmas);
        self.path.truncate(ptr.path);
        self.lemmas.truncate(ptr.lemmas);
    }
}
