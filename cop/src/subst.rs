use crate::Rewind;
use alloc::vec::Vec;
use core::fmt::{self, Display};

/// Map from `usize` to `T` that can be efficiently restored to earlier states.
pub struct Subst<T> {
    /// `sub[i]` is `Some(t)` iff the variable `i` is substituted with `t`
    sub: Vec<Option<T>>,
    /// domain of the substitution, i.e. the indices `i` for which `sub[i].is_some()` holds
    dom: Vec<usize>,
    /// for all `i < dom_max`, `sub[i]` is defined, and
    /// for all `i`, `dom[i] < dom_max`
    dom_max: usize,
}

pub struct Ptr {
    dom_len: usize,
    dom_max: usize,
}

impl<T: Display> Display for Subst<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        "{{".fmt(f)?;
        let mut iter = self.dom.iter();
        if let Some(x) = iter.next() {
            write!(f, "{} ↦ {}", x, self.get(*x).unwrap())?;
            for x in iter {
                write!(f, ", {} ↦ {}", x, self.get(*x).unwrap())?;
            }
        }
        "}}".fmt(f)
    }
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
    /// Return the number of assigned variables.
    pub fn get_dom_len(&self) -> usize {
        self.dom.len()
    }

    /// Return the maximally assignable index.
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
        debug_assert_eq!(self.dom.len(), new_len)
    }

    /// Assign a given variable to the given term.
    ///
    /// Panic if the index exceeds the maximum domain.
    pub fn insert(&mut self, v: usize, tm: T) {
        self.sub[v] = Some(tm);
        self.dom.push(v);
    }

    /// Obtain a variable binding.
    ///
    /// Panic if the index exceeds the maximum domain.
    pub fn get(&self, v: usize) -> Option<&T> {
        self.sub[v].as_ref()
    }
}

impl<T: Copy> Subst<T> {
    /// Repeatedly apply substitution to a value.
    ///
    /// The function terminates as soon as either
    /// the given function returns no index or
    /// the substitution is not defined at the index.
    pub fn fix(&self, mut x: T, f: impl Fn(T) -> Option<usize>) -> T {
        loop {
            if let Some(i) = f(x) {
                match self.sub[i] {
                    Some(y) => x = y,
                    None => return x,
                }
            } else {
                return x;
            }
        }
    }
}

impl Ptr {
    pub fn dom_max(&self) -> usize {
        self.dom_max
    }
}

impl<T> From<&Subst<T>> for Ptr {
    fn from(sub: &Subst<T>) -> Self {
        Self {
            dom_len: sub.get_dom_len(),
            dom_max: sub.get_dom_max(),
        }
    }
}

impl<T> Rewind<&Ptr> for Subst<T> {
    fn rewind(&mut self, state: &Ptr) {
        self.set_dom_len(state.dom_len);
        self.set_dom_max(state.dom_max);
    }
}
