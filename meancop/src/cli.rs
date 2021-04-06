use clap::Clap;
use cop::lean::Cuts;
use std::path::PathBuf;

/// Automated theorem prover for first-order logic with equality
///
/// This prover aims to explore
/// efficient implementation techniques for both
/// clausal and nonclausal connection calculi.
///
/// Set the environment variable "LOG" to "info", "debug", or "trace"
/// to obtain an increasingly detailed log.
#[derive(Clap)]
pub struct Cli {
    /// Disregard all alternatives when a branch is closed
    ///
    /// Equivalent to `--cuts rei`.
    #[clap(long)]
    cut: bool,

    /// Disregard alternatives when a branch is closed
    ///
    /// This option specifies when
    /// backtracking should be prevented (cut) once a branch is closed.
    /// We can cut based on the type of proof step, namely
    /// on reduction (R) or extension (E) steps.
    /// We distinguish inclusive and exclusive cuts.
    /// These two cut types differ in their behaviour when
    /// a branch (including all its ancestors) is closed:
    /// Inclusive cut (I) excludes any possibility of closing the branch differently, whereas
    /// exclusive cut (X) only permits for different steps at the branch root.
    /// Cuts on reduction steps (R) are always inclusive, so
    /// we distinguish inclusive and exclusive cut only for extension steps,
    /// calling them EI and EX.
    /// This option thus takes concatenations of "r", "ei", and "ex",
    /// for example "r", "ei", "ex", "rei", "rex".
    ///
    /// This option makes the search incomplete!
    #[clap(long)]
    cuts: Option<Cuts>,

    /// Enable conjecture-directed proof search
    #[clap(long)]
    pub conj: bool,

    /// Disable matrix sorting by number of paths
    #[clap(long)]
    pub nopaths: bool,

    /// Maximal depth for iterative deepening
    #[clap(long)]
    lim: Option<usize>,

    /// Write SZS output (such as proofs and error details) to given file
    #[clap(short)]
    pub output: Option<PathBuf>,

    /// Write proof search statistics in JSON format to given file
    #[clap(long)]
    pub stats: Option<PathBuf>,

    /// Path of the TPTP problem file
    pub file: PathBuf,
}

impl Cli {
    pub fn get_cuts(&self) -> Cuts {
        if self.cut {
            Cuts::max()
        } else {
            self.cuts.unwrap_or_default()
        }
    }

    pub fn output(&self, out: impl std::fmt::Display) -> Result<(), std::io::Error> {
        use std::io::Write;
        match &self.output {
            Some(o) => std::fs::write(o, out.to_string()),
            None => write!(std::io::stdout(), "{}", cop::szs::Output(out)),
        }
    }

    pub fn depths(&self) -> Box<dyn Iterator<Item = usize>> {
        match self.lim {
            Some(lim) => Box::new(1..lim),
            None => Box::new(1..),
        }
    }
}
