use crate::term::Args;
use crate::App;

pub type Lit<P, C, V> = App<P, Args<C, V>>;

impl<P, C, V> Lit<P, C, V> {
    pub fn is_ground(&self) -> bool {
        self.vars().next().is_none()
    }

    pub fn vars(&self) -> impl Iterator<Item = &V> {
        self.args().iter().flat_map(|a| a.vars())
    }
}
