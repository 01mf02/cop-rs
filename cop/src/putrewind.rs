use crate::Rewind;

/// Rewind a `T` either by
/// replacing it with some `T` or by
/// rewinding it with another rewinder for `T`.
///
/// This is useful for rewinding substitutions;
/// when using restricted backtracking, a substitution can be rewound using a fast rewinder, but
/// when using unrestricted backtracking, it is replaced with a previously saved substitution.
///
/// Note that it would be desirable to find a mechanism to
/// rewind substitutions unconditionally, making this type obsolete.
pub enum PutRewind<T, R> {
    /// rewind by replacing
    Put(T),
    /// rewind by rewinding
    Rewind(R),
}

impl<T: Rewind<R>, R> Rewind<PutRewind<T, R>> for T {
    fn rewind(&mut self, state: PutRewind<T, R>) {
        match state {
            PutRewind::Put(x) => *self = x,
            PutRewind::Rewind(r) => self.rewind(r),
        }
    }
}
