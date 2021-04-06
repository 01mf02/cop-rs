#[cfg(feature = "serde")]
use serde::Serialize;

/// Branch statistics.
#[derive(Clone, Default)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Stats<T> {
    /// Is the branch closed?
    pub closed: T,
    /// Has the root proof step of this branch been changed?
    pub root_changed: T,
    /// Has any descendant proof step of this branch been changed?
    pub descendant_changed: T,
}

impl Stats<bool> {
    pub fn new(root_changed: bool) -> Self {
        Self {
            closed: false,
            root_changed,
            descendant_changed: false,
        }
    }
}

impl core::iter::FromIterator<Stats<bool>> for Stats<usize> {
    fn from_iter<I: IntoIterator<Item = Stats<bool>>>(iter: I) -> Self {
        let mut stats = Stats::default();

        for i in iter {
            stats.closed += usize::from(i.closed);
            stats.root_changed += usize::from(i.root_changed);
            stats.descendant_changed += usize::from(i.descendant_changed);
        }
        stats
    }
}
