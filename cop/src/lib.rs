#![no_std]

extern crate alloc;

pub mod lean;

pub mod app;
pub mod args;
pub mod change;
pub mod ctx_iter;
pub mod equality;
pub mod fof;
pub mod literal;
pub mod offset;
pub mod ptr;
pub mod rewind;
pub mod role;
pub mod signed;
pub mod subst;
pub mod symbol;
pub mod szs;
pub mod term;

pub use app::App;
pub use args::Args;
pub use ctx_iter::CtxIter;
pub use fof::Form;
pub use literal::Lit;
pub use offset::Offset;
pub use rewind::Rewind;
pub use signed::Signed;
pub use symbol::Symbol;
pub use term::Term;

use alloc::vec::Vec;

fn keep_first<T: Eq>(v: impl Iterator<Item = T>) -> Vec<T> {
    let mut result = Vec::new();
    for x in v {
        if result.iter().all(|y| x != *y) {
            result.push(x)
        }
    }
    result
}

pub fn fold_right1<T>(mut vec: Vec<T>, f: impl Fn(T, T) -> T) -> Option<T> {
    match vec.pop() {
        None => None,
        Some(last) => Some(vec.into_iter().rev().fold(last, |acc, x| f(x, acc))),
    }
}

/// Return the keys that are mapped to more than one different value.
pub fn nonfunctional<K, V>(v: Vec<(K, V)>) -> impl Iterator<Item = K>
where
    K: Clone + Eq + core::hash::Hash,
    V: Clone + Eq,
{
    let map: hashbrown::HashMap<_, _> = v.iter().cloned().collect();
    v.into_iter().filter_map(move |(k, v1)| match map.get(&k) {
        Some(v2) if v1 != *v2 => Some(k),
        _ => None,
    })
}

/// Remove all elements of `v2` from `v1` and append `v2` to the result.
///
/// ~~~
/// # use cop::union1;
/// let mut v1 = vec![0, 2, 3, 3, 4, 4];
/// let v2 = vec![1, 2, 2, 4, 5];
/// union1(&mut v1, v2);
/// assert_eq!(v1, vec![0, 3, 3, 1, 2, 2, 4, 5])
/// ~~~
pub fn union1<T: Eq>(v1: &mut Vec<T>, mut v2: Vec<T>) {
    v1.retain(|x| !v2.iter().any(|y| x == y));
    v1.append(&mut v2)
}

/// Compute the union of two vectors, removing duplicates from the first one.
///
/// This function first removes duplicates from the first vector,
/// keeping the first occurence of each element.
/// It then removes from the first vector any element present in the second vector.
/// Finally, it reverses the first vector and concatenates it with the second vector.
///
/// This function is so complicated in order to
/// simulate the function `union2` of Otten's leanCoP.
///
/// ~~~
/// assert_eq!(cop::union2(vec![2, 1, 3, 4, 1], vec![2, 2]), vec![4, 3, 1, 2, 2]);
/// ~~~
pub fn union2<T: Eq>(v1: Vec<T>, mut v2: Vec<T>) -> Vec<T> {
    let v1_uniq = keep_first(v1.into_iter()).into_iter();
    let mut result: Vec<T> = v1_uniq.filter(|x| v2.iter().all(|y| x != y)).collect();
    result.reverse();
    result.append(&mut v2);
    result
}
