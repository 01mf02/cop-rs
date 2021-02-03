use super::contrapositive::OContrapositive as OContra;
use super::search::{Action, Context};
use crate::offset::{OLit, Offset, Sub};
use alloc::vec::Vec;
use core::fmt::{self, Display};
use core::ops::Neg;

pub enum Proof<'t, P, C> {
    Lem,
    Red,
    Ext(OContra<'t, P, C>, Vec<Self>),
}

impl<'t, P, C> Proof<'t, P, C> {
    pub fn from_iter(iter: &mut impl Iterator<Item = Action<'t, P, C>>, off: &mut usize) -> Self {
        match iter.next().unwrap() {
            Action::Prove => Self::Lem,
            Action::Reduce(_, _) => Self::Red,
            Action::Extend(_, cs, skip) => {
                let contra = &cs[skip];
                let ocontra = Offset::new(*off, contra);
                *off += contra.vars.map(|v| v + 1).unwrap_or(0);
                let proofs = contra.rest.iter().map(|_| Self::from_iter(iter, off));
                Self::Ext(ocontra, proofs.collect())
            }
        }
    }

    pub fn display(&self, lit: OLit<'t, P, C>) -> Disp<'_, 't, P, C> {
        let depth = 0;
        let proof = self;
        Disp { depth, lit, proof }
    }
}

impl<'t, P: Eq + Neg<Output = P> + Clone, C: Eq> Proof<'t, P, C> {
    pub fn check(&self, sub: &Sub<'t, C>, lit: OLit<'t, P, C>, mut ctx: Context<'t, P, C>) -> bool {
        match self {
            Proof::Lem => ctx.lemmas.iter().any(|lem| lit.eq_mod(sub, lem)),
            Proof::Red => ctx.path.iter().any(|path| lit.neg_eq_mod(sub, path)),
            Proof::Ext(ocontra, proofs) => {
                ctx.path.push(lit);
                let mut rest = ocontra.rest().into_iter().zip(proofs.iter());
                rest.all(|(clit, proof)| {
                    let result = proof.check(sub, clit, ctx.clone());
                    ctx.lemmas.push(clit);
                    result
                }) && ocontra.head() == &-lit.head().clone()
                    && ocontra.args().eq_mod(sub, lit.args())
            }
        }
    }
}

pub struct Disp<'p, 't, P, C> {
    depth: usize,
    lit: OLit<'t, P, C>,
    proof: &'p Proof<'t, P, C>,
}

impl<'p, 't, P: Display + Neg<Output = P> + Clone, C: Display> Display for Disp<'p, 't, P, C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{: <1$}", "", self.depth * 2)?;
        write!(f, "{} ", self.lit)?;
        match self.proof {
            Proof::Lem => writeln!(f, "Lem")?,
            Proof::Red => writeln!(f, "Red")?,
            Proof::Ext(contra, proofs) => {
                writeln!(f, "Ext {}{}", -(self.lit.head().clone()), contra)?;
                let depth = self.depth + 1;
                for (proof, lit) in proofs.iter().zip(contra.rest().into_iter()) {
                    Self { depth, lit, proof }.fmt(f)?
                }
            }
        }
        Ok(())
    }
}
