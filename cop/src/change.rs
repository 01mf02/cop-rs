//! Functions from `T` to `T` that return whether the output differs from the input.

/// Change we can believe in.
///
/// `true` if there has been a change, `false` if not.
pub type Change = bool;

/// If there should be change, apply function to value, else return unchanged value.
pub fn and_then<T>(c: Change, x: T, f: impl FnOnce(T) -> (Change, T)) -> (Change, T) {
    if c {
        f(x)
    } else {
        (false, x)
    }
}
