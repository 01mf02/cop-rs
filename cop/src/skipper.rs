#[derive(Copy, Clone)]
pub struct Skipper<T> {
    x: T,
    n: usize,
}

impl<T> Skipper<T> {
    pub fn new(x: T) -> Self {
        Self { x, n: 0 }
    }
}

impl<T: IntoIterator> Skipper<T> {
    pub fn iter(self) -> impl Iterator<Item = T::Item> {
        self.x.into_iter().skip(self.n)
    }
}

impl<T: IntoIterator + Clone> Iterator for Skipper<T> {
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.clone().iter().next().map(|next| {
            self.n += 1;
            next
        })
    }
}
