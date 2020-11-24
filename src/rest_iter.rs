/// Iterate over every element of a vector and the remaining vector.
///
/// ~~~
/// # use cop::RestIter;
/// let v = vec![1, 2, 3];
/// let r: Vec<_> = RestIter::from(v)
///     .map(|(x, r)| (x, r.into_iter().collect()))
///     .collect();
/// assert_eq!(r, vec![(1, vec![2, 3]), (2, vec![1, 3]), (3, vec![1, 2])])
/// ~~~
#[derive(Clone)]
pub struct RestIter<T> {
    left: Vec<T>,
    right: Vec<T>,
}

pub struct Rest<T>(RestIter<T>);

impl<T> IntoIterator for Rest<T> {
    type Item = T;
    type IntoIter = std::iter::Chain<std::vec::IntoIter<T>, std::iter::Rev<std::vec::IntoIter<T>>>;

    fn into_iter(self) -> Self::IntoIter {
        let left = self.0.left.into_iter();
        let right = self.0.right.into_iter().rev();
        left.chain(right)
    }
}

impl<T> From<Vec<T>> for RestIter<T> {
    fn from(mut v: Vec<T>) -> Self {
        v.reverse();
        Self {
            left: vec![],
            right: v,
        }
    }
}

impl<T: Clone> Iterator for RestIter<T> {
    type Item = (T, Rest<T>);
    fn next(&mut self) -> Option<Self::Item> {
        let mid = self.right.pop()?;
        let rest = Rest(self.clone());
        self.left.push(mid.clone());
        Some((mid, rest))
    }
}
