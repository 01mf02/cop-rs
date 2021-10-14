//! Vectors with backtracking support.
//!
//! Based on code originally written by Cezary Kaliszyk.

use alloc::vec::Vec;

pub struct VecBack<T> {
    data: Vec<(T, usize)>,
    back: Back,
}

#[derive(Copy, Clone, Default)]
pub struct Back {
    top: usize,
    last_saved: usize,
}

impl<T> VecBack<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.back.top == 0
    }

    pub fn push(&mut self, x: T) {
        let maybe_shorter = core::cmp::max(self.back.last_saved, self.back.top);
        self.data.truncate(maybe_shorter);
        self.data.push((x, self.back.top));
        self.back.top = self.data.len();
    }

    pub fn pop(&mut self) -> &T {
        let oldtop = self.back.top - 1;
        self.back.top = self.data[oldtop].1;
        &self.data[oldtop].0
    }

    pub fn save(&mut self) -> Back {
        self.back.last_saved = self.data.len();
        self.back
    }

    pub fn restore(&mut self, p: Back) {
        self.back = p;
        self.data.truncate(p.last_saved);
    }
}

impl<T> Default for VecBack<T> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            back: Default::default(),
        }
    }
}

#[test]
fn bla() {
    let mut v = VecBack::new();
    v.push(0);
    v.push(1);
    let ss1 = v.save();

    assert_eq!(*v.pop(), 1);
    assert_eq!(*v.pop(), 0);

    v.restore(ss1);
    assert_eq!(*v.pop(), 1);
    assert_eq!(*v.pop(), 0);
}

#[test]
fn bla2() {
    let mut v = VecBack::new();

    v.push(0);
    v.push(1);
    let ss1 = v.save();

    v.push(2);
    v.push(3);
    let ss2 = v.save();

    assert!(!v.is_empty());
    assert_eq!(*v.pop(), 3);
    assert_eq!(*v.pop(), 2);
    assert_eq!(*v.pop(), 1);

    v.restore(ss2);
    assert_eq!(*v.pop(), 3);
    assert_eq!(*v.pop(), 2);
    assert_eq!(*v.pop(), 1);
    assert_eq!(*v.pop(), 0);
    assert!(v.is_empty());

    v.restore(ss1);
    assert_eq!(*v.pop(), 1);
    assert_eq!(*v.pop(), 0);
    assert!(v.is_empty());
}
