use super::contrapositive::PreCp;
use super::search::{Action, Context, OLitMat};
use crate::offset::{Offset, Sub};
use alloc::vec::Vec;
use core::fmt::{self, Display};
use core::ops::Neg;

/// Offset contrapositive.
pub type OContra<'t, P, C> = Offset<&'t PreCp<'t, crate::Lit<P, C, usize>, usize>>;

/// A full proof for a litmat, including subproofs.
pub enum Proof<'t, P, C> {
    /// Lemma step
    Lem,
    /// Reduction step
    Red,
    /// Extension step, storing proofs for all items of the contrapositive
    Ext(OContra<'t, P, C>, Vec<Self>),
    /// Decomposition step, applicable if the litmat is a matrix
    ///
    /// This stores the index of the chosen clause in the matrix as well as
    /// a proof for every litmat in the clause.
    Dec(usize, Vec<Self>),
}

impl<'t, P, C> Proof<'t, P, C> {
    /// Produce a tree-like proof from a series of proof search actions.
    pub fn from_iter(iter: &mut impl Iterator<Item = Action<'t, P, C>>, off: &mut usize) -> Self {
        match iter.next().unwrap() {
            Action::Prove => Self::Lem,
            Action::Reduce(_, _) => Self::Red,
            Action::Extend(_, cs, skip) => {
                let contra = &cs[skip];
                let ocontra = Offset::new(*off, contra);
                *off += contra.offset.map(|v| v + 1).unwrap_or(0);
                let proofs = contra.into_iter().map(|_| Self::from_iter(iter, off));
                Self::Ext(ocontra, proofs.collect())
            }
            Action::Decompose(mat, skip) => {
                let cl = mat.into_iter().nth(skip).unwrap();
                let lms = cl.map(|cl| &cl.1).into_iter();
                let proofs = lms.map(|_| Self::from_iter(iter, off));
                Self::Dec(skip, proofs.collect())
            }
        }
    }

    /// Given a proof and the litmat it proves, make it into something that can be displayed.
    pub fn display(&self, lit: OLitMat<'t, P, C>) -> Disp<'_, 't, P, C> {
        let depth = 0;
        let proof = self;
        Disp { depth, lit, proof }
    }
}

impl<'t, P: Eq + Neg<Output = P> + Clone, C: Eq> Proof<'t, P, C> {
    /// Check the proof.
    pub fn check(
        &self,
        sub: &Sub<'t, C>,
        lit: OLitMat<'t, P, C>,
        mut ctx: Context<'t, P, C>,
    ) -> bool {
        use crate::LitMat::{Lit, Mat};
        match (self, lit.transpose()) {
            (Proof::Lem, Lit(lit)) => ctx.lemmas.iter().any(|lem| lit.eq_mod(sub, lem)),
            (Proof::Red, Lit(lit)) => ctx.path.iter().any(|path| lit.neg_eq_mod(sub, path)),
            (Proof::Ext(ocontra, proofs), Lit(lit)) => {
                ctx.path.push(lit);
                let rest = ocontra.into_iter();
                rest.zip(proofs.iter()).all(|(clit, proof)| {
                    let result = proof.check(sub, clit, ctx.clone());
                    if let Lit(clit) = clit.transpose() {
                        ctx.lemmas.push(clit);
                    }
                    result
                }) && ocontra.map(|cp| cp.contra.lit).neg_eq_mod(sub, &lit)
            }
            (Proof::Dec(skip, proofs), Mat(mat)) => {
                let cl = mat.into_iter().nth(*skip).unwrap();
                let lms = cl.map(|cl| &cl.1).into_iter();
                lms.zip(proofs.iter()).all(|(clit, proof)| {
                    let result = proof.check(sub, clit, ctx.clone());
                    if let Lit(clit) = clit.transpose() {
                        ctx.lemmas.push(clit);
                    }
                    result
                })
            }
            _ => panic!("proof step does not fit litmat"),
        }
    }
}

/// A proof together with additional information that can be displayed.
pub struct Disp<'p, 't, P, C> {
    depth: usize,
    lit: OLitMat<'t, P, C>,
    proof: &'p Proof<'t, P, C>,
}

impl<'p, 't, P: Display + Neg<Output = P> + Clone, C: Display> Display for Disp<'p, 't, P, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{: <1$}", "", self.depth * 2)?;
        write!(f, "{} ", self.lit)?;
        use crate::LitMat::{Lit, Mat};
        match (self.proof, self.lit.transpose()) {
            (Proof::Lem, Lit(_)) => writeln!(f, "Lem")?,
            (Proof::Red, Lit(_)) => writeln!(f, "Red")?,
            (Proof::Ext(contra, proofs), Lit(lit)) => {
                writeln!(f, "Ext {}{}", -lit.head().clone(), lit.args())?;
                let depth = self.depth + 1;
                for (proof, lit) in proofs.iter().zip(contra.into_iter()) {
                    Self { depth, lit, proof }.fmt(f)?
                }
            }
            (Proof::Dec(offset, proofs), Mat(mat)) => {
                writeln!(f, "Dec {}", offset)?;
                let depth = self.depth + 1;
                let cl = mat.into_iter().nth(*offset).unwrap();
                let lms = cl.map(|cl| &cl.1).into_iter();
                for (proof, lit) in proofs.iter().zip(lms) {
                    Self { depth, lit, proof }.fmt(f)?
                }
            }
            _ => panic!("proof step does not fit litmat"),
        }
        Ok(())
    }
}
