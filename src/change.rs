pub type DynFn<T> = dyn Fn(T) -> (Change, T);

/// `true` if there has been a change, `false` if not
pub type Change = bool;

pub fn fix<T>(mut x: T, f: impl Fn(T) -> (Change, T)) -> T {
    loop {
        let (change, y) = f(x);
        x = y;
        if !change {
            return x;
        }
    }
}

pub fn fold<T>(x: T, fs: &[Box<DynFn<T>>]) -> (Change, T) {
    fs.iter().fold((false, x), |(change, x), f| {
        let (change_y, y) = f(x);
        (change | change_y, y)
    })
}
