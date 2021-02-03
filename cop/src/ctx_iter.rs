use alloc::{vec, vec::Vec};

/// Iterate over all vector elements and their contexts, i.e. the remaining vector.
///
/// ~~~
/// # use cop::CtxIter;
/// let v = vec![1, 2, 3];
/// let r: Vec<_> = CtxIter::from(v)
///     .map(|(x, ctx)| (x, ctx.into_iter().collect()))
///     .collect();
/// assert_eq!(r, vec![(1, vec![2, 3]), (2, vec![1, 3]), (3, vec![1, 2])])
/// ~~~
pub struct CtxIter<T> {
    mid: Option<T>,
    ctx: Ctx<T>,
}

#[derive(Clone)]
pub struct Ctx<T> {
    left: Vec<T>,
    right: Vec<T>,
}

impl<T> Ctx<T> {
    fn shift(&mut self, mid: T) -> Option<T> {
        self.left.push(mid);
        self.right.pop()
    }
}

impl<T> From<Vec<T>> for Ctx<T> {
    fn from(mut right: Vec<T>) -> Self {
        let left = Vec::new();
        right.reverse();
        Ctx { left, right }
    }
}

impl<T> From<Vec<T>> for CtxIter<T> {
    fn from(v: Vec<T>) -> Self {
        let mut ctx = Ctx::from(v);
        let mid = ctx.right.pop();
        Self { mid, ctx }
    }
}

impl<T: Clone> Iterator for CtxIter<T> {
    type Item = (T, Ctx<T>);
    fn next(&mut self) -> Option<Self::Item> {
        let mid = self.mid.take()?;
        let ctx = self.ctx.clone();
        self.mid = self.ctx.shift(mid.clone());
        Some((mid, ctx))
    }
}

impl<T> IntoIterator for Ctx<T> {
    type Item = T;
    type IntoIter = core::iter::Chain<vec::IntoIter<T>, core::iter::Rev<vec::IntoIter<T>>>;

    fn into_iter(self) -> Self::IntoIter {
        let left = self.left.into_iter();
        let right = self.right.into_iter().rev();
        left.chain(right)
    }
}
