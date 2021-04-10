use super::search::Action;
use alloc::vec::Vec;
#[cfg(feature = "serde")]
use serde::Serialize;

#[derive(Clone)]
pub struct Steps<'t, P, C> {
    steps: Vec<(Action<'t, P, C>, Change<bool>)>,
    next_replaced: bool,
}

/// Branch statistics.
#[derive(Clone, Default, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Change<T> {
    /// Has the root proof step of this branch been changed?
    pub root: T,
    /// Has any descendant proof step of this branch been changed?
    pub descendant: T,
}

impl Change<bool> {
    pub fn new(root: bool) -> Self {
        let descendant = false;
        Self { root, descendant }
    }

    pub fn any(&self) -> bool {
        self.root || self.descendant
    }
}

impl<'t, P, C> Steps<'t, P, C> {
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            next_replaced: false,
        }
    }

    /// Return the number of proof steps.
    pub fn len(&self) -> usize {
        self.steps.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Action<'t, P, C>> {
        self.steps.iter().map(|(step, _)| step)
    }

    pub fn changes(&self) -> impl Iterator<Item = &Change<bool>> {
        self.steps.iter().map(|(_, stats)| stats)
    }

    /// Add a new proof step.
    pub fn push(&mut self, action: Action<'t, P, C>) {
        let stats = Change::new(self.next_replaced);
        self.next_replaced = false;
        self.steps.push((action, stats));
    }

    /// Retain only the oldest `len` proof steps.
    pub fn truncate(&mut self, len: usize) {
        assert!(self.steps.len() >= len);
        self.remove(len);
        self.steps.truncate(len);
    }

    /// Update statistics for a proof step that is about to be removed.
    fn remove(&mut self, idx: usize) {
        // if the current proof step has replaced a previously closed branch,
        // then register that for the next potential proof step
        let changed = self.steps[idx].1.any();
        let closed = self.step_closed(idx);
        self.next_replaced = changed || closed;

        // if the current step is not closed, none of its ancestor steps is closed
        if closed {
            self.open_ancestors(idx)
        }
    }

    /// Register a descendant change for all open ancestors of the given proof step.
    fn open_ancestors(&mut self, mut idx: usize) {
        while let Some((parent_idx, open)) = self.parent_step(idx) {
            // if the branch is open
            if open > 0 {
                return;
            }

            // all ancestor proof steps must be extension steps
            assert!(matches!(self.steps[parent_idx].0, Action::Extend(_, _, _)));
            self.steps[parent_idx].1.descendant = true;
            idx = parent_idx;
        }
    }

    /// If the given proof step has a parent,
    /// return its index and the number of its open children.
    fn parent_step(&self, idx: usize) -> Option<(usize, usize)> {
        let mut children = 1;
        for (i, step) in self.steps.iter().take(idx).enumerate().rev() {
            let mc = step.0.max_children();
            if mc < children {
                children += 1;
                children -= mc;
            } else {
                return Some((i, mc - children));
            }
        }
        None
    }

    /// Is the proof step with given index closed?
    fn step_closed(&self, idx: usize) -> bool {
        let scan = |open: &mut usize, s: &Action<'t, P, C>| {
            *open += s.max_children();
            *open -= 1;
            Some(*open)
        };
        self.steps
            .iter()
            .skip(idx)
            .map(|(step, _)| step)
            .scan(1, scan)
            .any(|open| open == 0)
    }
}
