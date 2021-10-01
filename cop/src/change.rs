//! Functions from `T` to `T` that return whether the output differs from the input.

/// Change function.
///
/// Takes an input and returns some output, as well as
/// whether the output is different from the input value.
pub type DynFn<T> = dyn Fn(T) -> (Change, T);

/// Change we can believe in.
///
/// `true` if there has been a change, `false` if not.
pub type Change = bool;

/// Apply a change function to a value as long as the function reports change.
pub fn fix<T>(mut x: T, f: impl Fn(T) -> (Change, T)) -> T {
    loop {
        let (change, y) = f(x);
        x = y;
        if !change {
            return x;
        }
    }
}

/// If there should be change, apply function to value, else return unchanged value.
pub fn and_then<T>(c: Change, x: T, f: impl FnOnce(T) -> (Change, T)) -> (Change, T) {
    if c {
        f(x)
    } else {
        (false, x)
    }
}
