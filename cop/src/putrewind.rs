use crate::Rewind;

pub enum PutRewind<T, R> {
    Put(T),
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
