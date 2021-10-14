//! Stacks with backtracking support.
//!
//! Based on code originally written by Cezary Kaliszyk.

use alloc::vec::Vec;

pub struct StackBack<T> {
    data: Vec<(T, usize)>,
    back: Back,
}

#[derive(Copy, Clone, Default)]
pub struct Back {
    top: usize,
    last_saved: usize,
}

impl<T> StackBack<T> {
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

    pub fn pop(&mut self) -> Option<&T> {
        next(&self.data, &mut self.back.top)
    }

    pub fn save(&mut self) -> Back {
        self.back.last_saved = self.data.len();
        self.back
    }

    pub fn restore(&mut self, p: Back) {
        self.back = p;
        self.data.truncate(p.last_saved);
    }

    pub fn iter(&self) -> Iter<T> {
        Iter {
            data: &self.data,
            pos: self.back.top,
        }
    }

    pub fn len(&self) -> usize {
        self.iter().count()
    }
}

fn next<'a, T>(data: &'a [(T, usize)], pos: &mut usize) -> Option<&'a T> {
    (*pos > 0).then(|| {
        let oldpos = *pos - 1;
        *pos = data[oldpos].1;
        &data[oldpos].0
    })
}

impl<T> Default for StackBack<T> {
    fn default() -> Self {
        Self {
            data: Vec::new(),
            back: Default::default(),
        }
    }
}

impl<'a, T> IntoIterator for &'a StackBack<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

pub struct Iter<'a, T> {
    data: &'a [(T, usize)],
    pos: usize,
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        next(&self.data, &mut self.pos)
    }
}

#[test]
fn bla() {
    let mut s = StackBack::new();
    s.push(0);
    s.push(1);
    let ss1 = s.save();

    assert_eq!(s.pop(), Some(&1));
    assert_eq!(s.pop(), Some(&0));
    assert_eq!(s.pop(), None);

    s.restore(ss1);
    assert_eq!(s.pop(), Some(&1));
    assert_eq!(s.pop(), Some(&0));
    assert_eq!(s.pop(), None);
}

#[test]
fn bla2() {
    let mut s = StackBack::new();

    s.push(0);
    s.push(1);
    let ss1 = s.save();

    s.push(2);
    s.push(3);
    let ss2 = s.save();

    assert!(!s.is_empty());
    assert_eq!(s.pop(), Some(&3));
    assert_eq!(s.pop(), Some(&2));
    assert_eq!(s.pop(), Some(&1));

    s.restore(ss2);
    assert_eq!(s.pop(), Some(&3));
    assert_eq!(s.pop(), Some(&2));
    assert_eq!(s.pop(), Some(&1));
    assert_eq!(s.pop(), Some(&0));
    assert_eq!(s.pop(), None);
    assert!(s.is_empty());

    s.restore(ss1);
    assert_eq!(s.pop(), Some(&1));
    assert_eq!(s.pop(), Some(&0));
    assert_eq!(s.pop(), None);
    assert!(s.is_empty());
}

#[test]
fn iter() {
    let mut s = StackBack::new();

    s.push(0);
    s.push(1);
    assert_eq!(s.len(), 2);

    let v: Vec<&usize> = s.iter().collect();
    assert_eq!(v, Vec::from([&1, &0]));
}
