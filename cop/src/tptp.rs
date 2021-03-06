use crate::fof::{Form, Op, OpA, Quantifier};
use crate::role::{Role, RoleMap};
use crate::szs::NoSuccessKind;
use crate::term::{Args, Term};
use alloc::string::ToString;
use alloc::{boxed::Box, string::String, vec::Vec};
use tptp::{cnf, common, fof, top, TPTPIterator};

pub type STerm = Term<String, String>;
pub type SArgs = Args<String, String>;
pub type SForm = Form<String, String, String>;

pub fn parse<F>(bytes: &[u8], forms: &mut RoleMap<Vec<SForm>>, f: F) -> Result<(), NoSuccessKind>
where
    F: Fn(&str, &mut RoleMap<Vec<SForm>>) -> Result<(), NoSuccessKind>,
{
    let mut parser = TPTPIterator::<()>::new(&bytes);
    for input in &mut parser {
        let input = input.map_err(|_| NoSuccessKind::SyntaxError)?;
        match input {
            top::TPTPInput::Include(include) => f(include.file_name.0 .0, forms)?,
            top::TPTPInput::Annotated(ann) => {
                let (role, formula) = get_role_formula(*ann);
                log::info!("formula: {}", formula);
                forms.get_mut(role).push(formula);
            }
        };
    }
    if parser.remaining.is_empty() {
        Ok(())
    } else {
        Err(NoSuccessKind::SyntaxError)
    }
}

fn get_role_formula(annotated: top::AnnotatedFormula) -> (Role, SForm) {
    use top::AnnotatedFormula::*;
    match annotated {
        Fof(fof) => (Role::from(fof.0.role), SForm::from(*fof.0.formula)),
        Cnf(cnf) => (Role::from(cnf.0.role), SForm::from(*cnf.0.formula)),
    }
}

impl From<fof::FunctionTerm<'_>> for STerm {
    fn from(tm: fof::FunctionTerm) -> Self {
        use fof::FunctionTerm::*;
        match tm {
            Plain(p) => Self::from(p),
            Defined(d) => Self::from(d),
            System(_) => todo!(),
        }
    }
}

impl From<fof::DefinedTerm<'_>> for STerm {
    fn from(tm: fof::DefinedTerm) -> Self {
        use fof::DefinedTerm::*;
        match tm {
            Defined(d) => Self::from(d),
            Atomic(_) => todo!(),
        }
    }
}

impl From<tptp::common::DefinedTerm<'_>> for STerm {
    fn from(tm: tptp::common::DefinedTerm) -> Self {
        use tptp::common::DefinedTerm::*;
        match tm {
            Number(n) => Self::C(n.to_string(), Args::new()),
            Distinct(_) => todo!(),
        }
    }
}

impl From<fof::Term<'_>> for STerm {
    fn from(tm: fof::Term) -> Self {
        use fof::Term::*;
        match tm {
            Variable(v) => Self::V(v.to_string()),
            Function(f) => Self::from(*f),
        }
    }
}

impl From<fof::Arguments<'_>> for SArgs {
    fn from(args: fof::Arguments) -> Self {
        args.0.into_iter().map(Term::from).collect()
    }
}

impl From<fof::PlainTerm<'_>> for STerm {
    fn from(tm: fof::PlainTerm) -> Self {
        use fof::PlainTerm::*;
        match tm {
            Constant(c) => Self::C(c.to_string(), Args::new()),
            Function(f, args) => Self::C(f.to_string(), Args::from(*args)),
        }
    }
}

impl From<fof::LogicFormula<'_>> for SForm {
    fn from(frm: fof::LogicFormula) -> Self {
        use fof::LogicFormula::*;
        match frm {
            Binary(b) => Self::from(b),
            Unary(u) => Self::from(u),
            Unitary(u) => Self::from(u),
        }
    }
}

impl From<fof::QuantifiedFormula<'_>> for SForm {
    fn from(frm: fof::QuantifiedFormula) -> Self {
        let q = Quantifier::from(frm.quantifier);
        let vs = frm.bound.0.iter().rev().map(|v| v.to_string());
        vs.fold(Self::from(*frm.formula), |fm, v| Self::quant(q, v, fm))
    }
}

impl From<fof::UnitFormula<'_>> for SForm {
    fn from(frm: fof::UnitFormula) -> Self {
        use fof::UnitFormula::*;
        match frm {
            Unitary(u) => Self::from(u),
            Unary(u) => Self::from(u),
        }
    }
}

impl From<fof::InfixUnary<'_>> for SForm {
    fn from(frm: fof::InfixUnary) -> Self {
        let _: common::InfixInequality = frm.op;
        -Self::EqTm(Term::from(*frm.left), Term::from(*frm.right))
    }
}

impl From<fof::UnaryFormula<'_>> for SForm {
    fn from(frm: fof::UnaryFormula) -> Self {
        use fof::UnaryFormula::*;
        match frm {
            // negation
            Unary(_negation, fuf) => -Self::from(*fuf),
            // term inequality
            InfixUnary(i) => Self::from(i),
        }
    }
}

impl From<fof::BinaryFormula<'_>> for SForm {
    fn from(frm: fof::BinaryFormula) -> Self {
        use fof::BinaryFormula::*;
        match frm {
            Nonassoc(fbn) => Self::from(fbn),
            Assoc(fba) => Self::from(fba),
        }
    }
}

impl From<fof::BinaryNonassoc<'_>> for SForm {
    fn from(frm: fof::BinaryNonassoc) -> Self {
        let left = Box::new(Self::from(*frm.left));
        let right = Box::new(Self::from(*frm.right));
        use common::NonassocConnective::*;
        match frm.op {
            LRImplies => Self::Bin(left, Op::Impl, right),
            RLImplies => Self::Bin(right, Op::Impl, left),
            Equivalent => Self::Bin(left, Op::EqFm, right),
            NotEquivalent => -Self::Bin(left, Op::EqFm, right),
            NotOr => -(*left | *right),
            NotAnd => -(*left & *right),
        }
    }
}

impl From<fof::BinaryAssoc<'_>> for SForm {
    fn from(fm: fof::BinaryAssoc) -> Self {
        use fof::BinaryAssoc::*;
        let (op, fms) = match fm {
            Or(fms) => (OpA::Disj, fms.0),
            And(fms) => (OpA::Conj, fms.0),
        };
        Self::BinA(op, fms.into_iter().map(Self::from).collect())
    }
}

impl From<fof::Quantifier> for Quantifier {
    fn from(q: fof::Quantifier) -> Self {
        use fof::Quantifier::*;
        match q {
            Forall => Self::Forall,
            Exists => Self::Exists,
        }
    }
}

impl From<fof::UnitaryFormula<'_>> for SForm {
    fn from(frm: fof::UnitaryFormula) -> Self {
        use fof::UnitaryFormula::*;
        match frm {
            Parenthesised(flf) => Self::from(*flf),
            Quantified(fqf) => Self::from(fqf),
            Atomic(a) => Self::from(*a),
        }
    }
}

impl From<fof::PlainAtomicFormula<'_>> for SForm {
    fn from(frm: fof::PlainAtomicFormula) -> Self {
        use fof::PlainTerm::*;
        match frm.0 {
            Constant(c) => Self::Atom(c.to_string(), Args::new()),
            Function(f, args) => Self::Atom(f.to_string(), Args::from(*args)),
        }
    }
}

impl From<fof::DefinedAtomicFormula<'_>> for SForm {
    fn from(frm: fof::DefinedAtomicFormula) -> Self {
        use fof::DefinedAtomicFormula::*;
        match frm {
            Plain(p) => Self::from(p),
            Infix(i) => Self::EqTm(Term::from(*i.left), Term::from(*i.right)),
        }
    }
}

impl From<fof::DefinedPlainFormula<'_>> for SForm {
    fn from(fm: fof::DefinedPlainFormula) -> Self {
        use fof::DefinedPlainTerm::Constant;
        match fm.0 {
            Constant(c) if c.0 .0 .0 .0 .0 == "true" => {
                let p = Self::Atom("$true".to_string(), Args::new());
                Self::imp(p.clone(), p)
            }
            Constant(c) if c.0 .0 .0 .0 .0 == "false" => {
                let p = Self::Atom("$false".to_string(), Args::new());
                p.clone() & -p
            }
            _ => todo!(),
        }
    }
}

impl From<fof::AtomicFormula<'_>> for SForm {
    fn from(frm: fof::AtomicFormula) -> Self {
        use fof::AtomicFormula::*;
        match frm {
            Plain(p) => Self::from(p),
            Defined(d) => Self::from(d),
            System(_) => todo!(),
        }
    }
}

impl From<fof::Formula<'_>> for SForm {
    fn from(frm: fof::Formula) -> Self {
        Self::from(frm.0)
    }
}

impl From<cnf::Literal<'_>> for SForm {
    fn from(lit: cnf::Literal) -> Self {
        use cnf::Literal::*;
        match lit {
            Atomic(a) => Self::from(a),
            NegatedAtomic(a) => -Self::from(a),
            Infix(i) => Self::from(i),
        }
    }
}

impl From<cnf::Disjunction<'_>> for SForm {
    fn from(frm: cnf::Disjunction) -> Self {
        Self::BinA(OpA::Disj, frm.0.into_iter().map(Self::from).collect())
    }
}

impl From<cnf::Formula<'_>> for SForm {
    fn from(frm: cnf::Formula) -> Self {
        use cnf::Formula::*;
        match frm {
            Disjunction(d) | Parenthesised(d) => Self::from(d),
        }
    }
}

impl From<top::FormulaRole<'_>> for Role {
    fn from(role: top::FormulaRole<'_>) -> Self {
        match role.0 .0 {
            "conjecture" => Self::Conjecture,
            "negated_conjecture" => Self::NegatedConjecture,
            _ => Self::Other,
        }
    }
}
