/// Iterate over every element of a vector and the remaining vector.
///
/// ~~~
/// # use cop::RestIter;
/// let v = vec![1, 2, 3];
/// let r: Vec<_> = RestIter::from(v)
///     .map(|(x, r)| (x, r.into_iter().collect()))
///     .collect();
/// assert_eq!(r, vec![(3, vec![1, 2]), (2, vec![1, 3]), (1, vec![2, 3])])
/// ~~~
#[derive(Clone)]
pub struct RestIter<T> {
    left: Vec<T>,
    right: Vec<T>,
}

pub struct Rest<T>(RestIter<T>);

impl<T> IntoIterator for Rest<T> {
    type Item = T;
    type IntoIter = std::iter::Chain<std::vec::IntoIter<T>, std::iter::Skip<std::iter::Rev<std::vec::IntoIter<T>>>>;

    fn into_iter(self) -> Self::IntoIter {
        let right = self.0.right.into_iter().rev().skip(1);
        self.0.left.into_iter().chain(right)
    }
}

impl<T> From<Vec<T>> for RestIter<T> {
    fn from(v: Vec<T>) -> Self {
        Self {
            left: v,
            right: vec![],
        }
    }
}

impl<T: Clone> Iterator for RestIter<T> {
    type Item = (T, Rest<T>);
    fn next(&mut self) -> Option<Self::Item> {
        let mid = self.left.pop()?;
        self.right.push(mid.clone());
        Some((mid, Rest(self.clone())))
    }
}
