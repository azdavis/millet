//! Checking if a type is an equality type. TODO use

#![allow(dead_code)]

use crate::types::{BasicOverload, Overload, RecordTy, Subst, SubstEntry, Sym, Ty, TyVarKind};
use std::fmt;

/// A type that is not equality.
#[derive(Debug)]
pub(crate) enum NotEqTy {
  RegularTyVar,
  Real,
  Fn,
}

impl fmt::Display for NotEqTy {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      NotEqTy::RegularTyVar => f.write_str("a regular type variable"),
      NotEqTy::Real => f.write_str("`real`"),
      NotEqTy::Fn => f.write_str("a function type"),
    }
  }
}

/// If the given type is an equality type, returns `None`. Else, returns a kind of type contained in
/// the given type that makes the given type not an equality type.
pub(crate) fn ck(subst: &Subst, ty: &Ty) -> Option<NotEqTy> {
  match ty {
    Ty::None => None,
    Ty::BoundVar(_) => panic!("need binders to determine if bound var is equality"),
    Ty::MetaVar(mv) => match subst.get(*mv) {
      None => Some(NotEqTy::RegularTyVar),
      Some(entry) => match entry {
        SubstEntry::Solved(ty) => ck(subst, ty),
        SubstEntry::Kind(kind) => match kind {
          TyVarKind::Equality => None,
          TyVarKind::Overloaded(ov) => match ov {
            Overload::Basic(basic) => ck_basic(*basic),
            Overload::Composite(comp) => comp.as_basics().iter().find_map(|&basic| ck_basic(basic)),
          },
          TyVarKind::Record(rows) => ck_record(subst, rows),
        },
      },
    },
    Ty::FixedVar(fv) => (!fv.ty_var().is_equality()).then_some(NotEqTy::RegularTyVar),
    Ty::Record(rows) => ck_record(subst, rows),
    Ty::Con(args, sym) => {
      // TODO arrays should be equality?
      if *sym == Sym::REAL {
        Some(NotEqTy::Real)
      } else if *sym == Sym::REF {
        None
      } else {
        args.iter().find_map(|ty| ck(subst, ty))
      }
    }
    Ty::Fn(_, _) => Some(NotEqTy::Fn),
  }
}

fn ck_record(subst: &Subst, rows: &RecordTy) -> Option<NotEqTy> {
  rows.values().find_map(|ty| ck(subst, ty))
}

/// NOTE: this is an optimization. The (ideally, if our assumptions are correct) equivalent but
/// slower thing to do would be iterate over all the real symbols for the basic overload and check
/// whether all of them are equality.
///
/// However, that should always return the same result as this because the signatures `INTEGER`,
/// `WORD`, `STRING`, and `CHAR` all have their primary types (e.g. `int` for `INTEGER`) as
/// `eqtype`s.
fn ck_basic(ov: BasicOverload) -> Option<NotEqTy> {
  match ov {
    BasicOverload::Int | BasicOverload::Word | BasicOverload::String | BasicOverload::Char => None,
    BasicOverload::Real => Some(NotEqTy::Real),
  }
}
