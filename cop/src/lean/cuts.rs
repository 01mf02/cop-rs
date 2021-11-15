use alloc::{format, string::String, string::ToString};
use core::convert::{TryFrom, TryInto};
use core::result::Result;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

#[derive(Copy, Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Cut {
    Exclusive,
    Inclusive,
}

#[derive(Copy, Clone, Default, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Cuts {
    /// perform (inclusive) cut on reduction steps
    pub reduction: bool,
    /// perform cut on extension steps
    pub extension: Option<Cut>,
    /// perform cut on decomposition steps
    pub decomposition: Option<Cut>,
}

impl Cuts {
    /// Return the strongest combination of cuts.
    pub fn max() -> Self {
        Self {
            reduction: true,
            extension: Some(Cut::Inclusive),
            decomposition: Some(Cut::Inclusive),
        }
    }

    /// Return true if backtracking using these cuts may grow the state
    /// (in particular path, lemmas, and promises).
    pub fn backtracking_may_grow(&self) -> bool {
        self.extension.is_none() || self.decomposition.is_none()
    }
}

impl TryFrom<char> for Cut {
    type Error = String;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            'x' => Ok(Cut::Exclusive),
            'i' => Ok(Cut::Inclusive),
            _ => Err(format!("unknown cut type: {}", c)),
        }
    }
}

impl core::str::FromStr for Cuts {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut cuts = Cuts::default();
        let mut s = s.chars();
        let get_cut = |c: Option<char>| -> Result<Cut, Self::Err> {
            c.ok_or_else(|| "cut type expected".to_string())?.try_into()
        };
        while let Some(c) = s.next() {
            match c {
                'r' => cuts.reduction = true,
                'e' => cuts.extension = Some(get_cut(s.next())?),
                'd' => cuts.decomposition = Some(get_cut(s.next())?),
                _ => return Err(format!("unknown proof step type: {}", c)),
            }
        }
        Ok(cuts)
    }
}
