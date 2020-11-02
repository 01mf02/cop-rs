pub struct Subst<T> {
    /// `sub[i]` is `Some(t)` iff the variable `i` is substituted with `t`
    sub: Vec<Option<T>>,
    /// domain of the substitution, i.e. the indices `i` for which `sub[i].is_some()` holds
    dom: Vec<usize>,
    dom_max: usize,
}

impl<T> Default for Subst<T> {
    fn default() -> Self {
        Self {
            sub: Vec::new(),
            dom: Vec::new(),
            dom_max: 0,
        }
    }
}

impl<T> Subst<T> {
    /// Make a new substitution, allowing for the substitution of variables until `c - 1`.
    fn with_capacity(c: usize) -> Self {
        Self {
            sub: Vec::with_capacity(c),
            dom: Vec::new(),
            dom_max: 0,
        }
    }

    /// Return the number of assigned variables.
    pub fn get_dom_len(&self) -> usize {
        self.dom.len()
    }

    pub fn get_dom_max(&self) -> usize {
        self.dom_max
    }

    /// Permit the assignment of variables at least up to (excluding) the given index.
    pub fn set_dom_max(&mut self, dom_max: usize) {
        if self.sub.len() < dom_max {
            self.sub.resize_with(dom_max, Default::default)
        }
        self.dom_max = dom_max;
    }

    /// Omit the most recent variable bindings to keep only the given number of bindings.
    pub fn set_dom_len(&mut self, new_len: usize) {
        for v in self.dom.drain(new_len..) {
            self.sub[v] = None
        }
    }

    /// Assign a given variable to the given term.
    ///
    /// Panic if the index exceeds the maximum domain.
    pub fn insert(&mut self, v: usize, tm: T) {
        self.dom.push(v);
        self.sub[v] = Some(tm);
    }

    /// Obtain a variable binding.
    ///
    /// Panic if the index exceeds the maximum domain.
    pub fn get(&self, v: usize) -> Option<&T> {
        self.sub[v].as_ref()
    }
}
