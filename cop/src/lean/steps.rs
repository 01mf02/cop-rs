use super::search::Action;
use super::Stats;
use alloc::vec::Vec;

#[derive(Clone)]
pub struct Steps<'t, P, C> {
    steps: Vec<(Action<'t, P, C>, Stats<bool>)>,
    next_replaced: bool,
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

    pub fn iter(&self) -> impl Iterator<Item = &(Action<'t, P, C>, Stats<bool>)> {
        self.steps.iter()
    }

    /// Add a new proof step.
    pub fn push(&mut self, action: Action<'t, P, C>) {
        let stats = Stats::new(self.next_replaced);
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
        let stats = &self.steps[idx].1;
        let changed = stats.root_changed || stats.descendant_changed;
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
            self.steps[parent_idx].1.descendant_changed = true;
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
