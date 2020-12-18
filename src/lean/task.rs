use crate::offset::OLit;
use crate::Lit;

pub type OClause<'t, P, C> = super::clause::OClause<'t, Lit<P, C, usize>>;

pub struct Task<'t, P, C> {
    cl: OClause<'t, P, C>,
    cl_skip: usize,
}

impl<'t, P, C> Task<'t, P, C> {
    pub fn new(cl: OClause<'t, P, C>) -> Self {
        Self { cl, cl_skip: 0 }
    }

    pub fn lits(&self) -> impl Iterator<Item = OLit<'t, P, C>> {
        self.cl.into_iter().skip(self.cl_skip)
    }
}

impl<'t, P, C> Clone for Task<'t, P, C> {
    fn clone(&self) -> Self {
        Self {
            cl: self.cl,
            cl_skip: self.cl_skip,
        }
    }
}

impl<'t, P, C> Copy for Task<'t, P, C> {}

impl<'t, P, C> Iterator for Task<'t, P, C> {
    type Item = OLit<'t, P, C>;
    fn next(&mut self) -> Option<Self::Item> {
        self.lits().next().map(|next| {
            self.cl_skip += 1;
            next
        })
    }
}
