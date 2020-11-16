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

/// A stack capable of storing and loading snapshots of all its elements.
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

    /// Return true if there are no snapshots that can be loaded.
    pub fn is_empty(&self) -> bool {
        self.bot.is_empty()
    }

    /// Store current elements as a new snapshot.
    pub fn store(&mut self) {
        self.bot.push(core::mem::take(&mut self.top));
    }

    /// Load elements of previous snapshot if there is one.
    pub fn load(&mut self) {
        if let Some(bot) = self.bot.pop() {
            self.top = bot
        }
    }

    /// Keep only given number of oldest snapshots.
    pub fn truncate(&mut self, len: usize) {
        self.bot.truncate(len + 1);
        self.load()
    }

    /// Push an element onto the current stack.
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
    /// Pop an element from the current stack.
    pub fn pop(&mut self) -> Option<T> {
        self.top.vec.pop().or_else(|| self.pop_bot().cloned())
    }
}

#[cfg(test)]
#[test]
fn usage() {
    let mut bts = BackTrackStack::new();
    assert!(bts.is_empty());
    bts.push(0);
    bts.push(1);
    bts.store();
    assert_eq!(bts.len(), 1);
    // 0 1

    assert_eq!(bts.pop(), Some(1));
    bts.push(2);
    bts.push(3);
    bts.store();
    assert_eq!(bts.len(), 2);
    // 0 1
    // 0   2 3

    assert_eq!(bts.pop(), Some(3));
    bts.push(4);
    bts.store();
    assert_eq!(bts.len(), 3);
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

    assert_eq!(bts.len(), 3);
    assert_eq!(bts.pop(), Some(5));
    assert_eq!(bts.pop(), Some(0));
    assert_eq!(bts.pop(), None);
    bts.load();

    assert_eq!(bts.len(), 2);
    assert_eq!(bts.pop(), Some(4));
    assert_eq!(bts.pop(), Some(2));
    assert_eq!(bts.pop(), Some(0));
    assert_eq!(bts.pop(), None);
    bts.load();

    assert_eq!(bts.len(), 1);
    assert_eq!(bts.pop(), Some(3));
    assert_eq!(bts.pop(), Some(2));
    assert_eq!(bts.pop(), Some(0));
    assert_eq!(bts.pop(), None);
    bts.load();

    assert!(bts.is_empty(), 0);
    assert_eq!(bts.pop(), Some(1));
    assert_eq!(bts.pop(), Some(0));
    assert_eq!(bts.pop(), None);
}
