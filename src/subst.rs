use crate::offset::OTerm;

pub struct Subst<T> {
    /// `sub[i]` is `Some(t)` iff the variable `i` is substituted with `t`
    sub: Vec<Option<T>>,
    /// domain of the substitution, i.e. the indices `i` for which `sub[i].is_some()` holds
    dom: Vec<usize>,
}

impl<T> Subst<T> {
    /// Make a new substitution, allowing for the substitution of variables until `c - 1`.
    fn with_capacity(c: usize) -> Self {
        Self {
            sub: Vec::with_capacity(c),
            dom: Vec::new(),
        }
    }

    /// Return the number of assigned variables.
    fn size(&self) -> usize {
        self.dom.len()
    }

    /// Omit the most recent variable bindings to keep only the given number of bindings.
    fn shrink_to(&mut self, new_len: usize) {
        for v in self.dom.drain(new_len..) {
            self.sub[v] = None
        }
    }

    /// Permit the assignment of variables up to (excluding) the given index.
    fn grow_to(&mut self, new_len: usize) {
        self.sub.resize_with(new_len, Default::default)
    }

    /// Assign a given variable to the given term.
    ///
    /// Panic if the variable is out of bounds.
    pub fn insert(&mut self, v: usize, tm: T) {
        self.dom.push(v);
        self.sub[v] = Some(tm);
    }

    /// Obtain a variable binding.
    ///
    /// Panic if the variable is out of bounds.
    pub fn get(&self, v: usize) -> Option<&T> {
        self.sub[v].as_ref()
    }
}
