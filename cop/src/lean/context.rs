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

impl<T: Rewind<R>, R> Rewind<Context<R>> for Context<T> {
    fn rewind(&mut self, ptr: Context<R>) {
        self.path.rewind(ptr.path);
        self.lemmas.rewind(ptr.lemmas);
    }
}
