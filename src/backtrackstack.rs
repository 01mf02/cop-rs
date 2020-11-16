#[derive(Default)]
struct Offset {
    /// how many snapshots back
    outer: usize,
    /// how many elements back in the given snapshot
    inner: usize,
}

struct OffVec<T> {
    off: Offset,
    vec: Vec<T>,
}

impl<T> Default for OffVec<T> {
    fn default() -> Self {
        Self {
            off: Offset::default(),
            vec: Vec::new(),
        }
    }
}

impl<T> Default for BackTrackStack<T> {
    fn default() -> Self {
        Self::new()
    }
}

pub struct BackTrackStack<T> {
    top: OffVec<T>,
    bot: Vec<OffVec<T>>,
}

/// A stack capable of storing and restoring snapshots of all its elements.
impl<T> BackTrackStack<T> {
    pub fn new() -> Self {
        Self {
            top: OffVec::default(),
            bot: Vec::new(),
        }
    }

    /// Return number of snapshots.
    pub fn len(&self) -> usize {
        self.bot.len()
    }

    /// Return true if there are no snapshots that can be restored.
    pub fn is_empty(&self) -> bool {
        self.bot.is_empty()
    }

    /// Create a new snapshot.
    pub fn track(&mut self) {
        self.bot.push(core::mem::take(&mut self.top));
    }

    /// Restore previous snapshot.
    pub fn back(&mut self) {
        if let Some(bot) = self.bot.pop() {
            self.top = bot
        }
    }

    /// Keep only given number of oldest snapshots.
    pub fn truncate(&mut self, len: usize) {
        self.bot.truncate(len + 1);
        self.back()
    }

    /// Push an element onto the latest snapshot.
    pub fn push(&mut self, x: T) {
        self.top.vec.push(x)
    }

    fn pop_bot(&mut self) -> Option<&T> {
        let off = &mut self.top.off;
        loop {
            let bot = self.bot.iter().rev().nth(off.outer)?;
            match bot.vec.iter().rev().nth(off.inner) {
                Some(y) => {
                    off.inner += 1;
                    return Some(y);
                }
                None => {
                    off.outer += bot.off.outer + 1;
                    off.inner = bot.off.inner
                }
            }
        }
    }
}

impl<T: Clone> BackTrackStack<T> {
    /// Pop an element from the latest snapshot.
    pub fn pop(&mut self) -> Option<T> {
        self.top.vec.pop().or_else(|| self.pop_bot().cloned())
    }
}

#[cfg(test)]
#[test]
fn usage() {
    let mut bts = BackTrackStack::new();
    bts.push(0);
    bts.push(1);
    bts.track();
    // 0 1

    assert_eq!(bts.pop(), Some(1));
    bts.push(2);
    bts.push(3);
    bts.track();
    // 0 1
    // 0   2 3

    assert_eq!(bts.pop(), Some(3));
    bts.push(4);
    bts.track();
    // 0 1
    // 0   2 3
    // 0   2   4

    assert_eq!(bts.pop(), Some(4));
    assert_eq!(bts.pop(), Some(2));
    bts.push(5);
    // 0 1
    // 0   2 3
    // 0   2   4
    // 0         5

    assert_eq!(bts.pop(), Some(5));
    assert_eq!(bts.pop(), Some(0));
    assert_eq!(bts.pop(), None);
    assert_eq!(bts.back(), true);

    assert_eq!(bts.pop(), Some(4));
    assert_eq!(bts.pop(), Some(2));
    assert_eq!(bts.pop(), Some(0));
    assert_eq!(bts.pop(), None);
    assert_eq!(bts.back(), true);

    assert_eq!(bts.pop(), Some(3));
    assert_eq!(bts.pop(), Some(2));
    assert_eq!(bts.pop(), Some(0));
    assert_eq!(bts.pop(), None);
    assert_eq!(bts.back(), true);

    assert_eq!(bts.pop(), Some(1));
    assert_eq!(bts.pop(), Some(0));
    assert_eq!(bts.pop(), None);
    assert_eq!(bts.back(), false);
}
