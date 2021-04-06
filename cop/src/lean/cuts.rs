use alloc::{format, string::String, string::ToString};
use core::convert::{TryFrom, TryInto};
use core::result::Result;

#[derive(Copy, Clone, Debug)]
pub enum Cut {
    Exclusive,
    Inclusive,
}

#[derive(Copy, Clone, Default, Debug)]
pub struct Cuts {
    /// perform (inclusive) cut on reduction steps
    pub reduction: bool,
    /// perform cut on extension steps
    pub extension: Option<Cut>,
}

impl Cuts {
    /// Return the strongest combination of cuts.
    pub fn max() -> Self {
        Self {
            reduction: true,
            extension: Some(Cut::Inclusive),
        }
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
        while let Some(c) = s.next() {
            match c {
                'r' => cuts.reduction = true,
                'e' => {
                    let c = s.next().ok_or_else(|| "cut type expected".to_string())?;
                    cuts.extension = Some(c.try_into()?);
                }
                _ => return Err(format!("unknown proof step type: {}", c)),
            }
        }
        Ok(cuts)
    }
}
