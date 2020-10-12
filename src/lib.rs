pub mod lean;

pub mod fof;
pub mod role;
pub mod term;

fn keep_first<T: Eq>(v: impl Iterator<Item = T>) -> Vec<T> {
    let mut result = vec![];
    for x in v {
        if result.iter().all(|y| x != *y) {
            result.push(x)
        }
    }
    result
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

fn union3<T: Eq>(v2: &mut Vec<T>, v1: Vec<T>) {
    for x in v1.into_iter() {
        if !v2.iter().any(|y| &x == y) {
            v2.push(x)
        }
    }
}
