use crate::term::Args;
use crate::App;

pub type Lit<P, C, V> = App<P, Args<C, V>>;

impl<P, C, V: Ord> Lit<P, C, V> {
    pub fn max_var(&self) -> Option<&V> {
        self.args().max_var()
    }
}
