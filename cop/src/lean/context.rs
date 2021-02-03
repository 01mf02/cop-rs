use crate::Rewind;
use alloc::vec::Vec;

#[derive(Clone, Default)]
pub struct Context<T> {
    pub path: T,
    pub lemmas: T,
}

#[derive(Copy, Clone)]
pub struct Ptr {
    path_len: usize,
    lemmas_len: usize,
}

impl<'t, T> From<&Context<Vec<T>>> for Ptr {
    fn from(ctx: &Context<Vec<T>>) -> Self {
        Self {
            path_len: ctx.path.len(),
            lemmas_len: ctx.lemmas.len(),
        }
    }
}

impl<T> Rewind<Ptr> for Context<Vec<T>> {
    fn rewind(&mut self, ptr: Ptr) {
        assert!(self.path.len() >= ptr.path_len);
        assert!(self.lemmas.len() >= ptr.lemmas_len);
        self.path.truncate(ptr.path_len);
        self.lemmas.truncate(ptr.lemmas_len);
    }
}
