//! Checking if a type is an equality type. TODO use

#![allow(dead_code)]

use crate::types::{BasicOverload, Overload, RecordTy, Subst, SubstEntry, Sym, Ty, TyVarKind};

/// A reason why something was not an equality type.
pub(crate) enum NotEqReason {
  TyVar,
  Real,
  Fn,
  Unknown,
}

/// Returns the reason why this is **not** an equality type, or None if it **is** an equality type.
pub(crate) fn ck(subst: &Subst, ty: &Ty) -> Option<NotEqReason> {
  match ty {
    Ty::None => Some(NotEqReason::Unknown),
    Ty::BoundVar(_) => panic!("need binders to determine if bound var is equality"),
    Ty::MetaVar(mv) => match subst.get(*mv)? {
      SubstEntry::Solved(ty) => ck(subst, ty),
      SubstEntry::Kind(kind) => match kind {
        TyVarKind::Equality => None,
        TyVarKind::Overloaded(ov) => match ov {
          Overload::Basic(basic) => ck_basic_overload(*basic),
          Overload::Composite(comp) => {
            comp.as_basics().iter().find_map(|&basic| ck_basic_overload(basic))
          }
        },
        TyVarKind::Record(rows) => ck_record(subst, rows),
      },
    },
    Ty::FixedVar(fv) => (!fv.ty_var().is_equality()).then_some(NotEqReason::TyVar),
    Ty::Record(rows) => ck_record(subst, rows),
    Ty::Con(args, sym) => {
      // TODO arrays should be equality?
      if *sym == Sym::REAL {
        Some(NotEqReason::Real)
      } else if *sym == Sym::REF {
        None
      } else {
        args.iter().find_map(|ty| ck(subst, ty))
      }
    }
    Ty::Fn(_, _) => Some(NotEqReason::Fn),
  }
}

fn ck_record(subst: &Subst, rows: &RecordTy) -> Option<NotEqReason> {
  rows.values().find_map(|ty| ck(subst, ty))
}

/// NOTE: this is an optimization. The (ideally, if our assumptions are correct) equivalent but
/// slower thing to do would be iterate over all the real symbols for the basic overload and check
/// whether all of them are equality.
///
/// However, that should always return the same result as this because the signatures `INTEGER`,
/// `WORD`, `STRING`, and `CHAR` all have their primary types (e.g. `int` for `INTEGER`) as
/// `eqtype`s.
fn ck_basic_overload(ov: BasicOverload) -> Option<NotEqReason> {
  match ov {
    BasicOverload::Int | BasicOverload::Word | BasicOverload::String | BasicOverload::Char => None,
    BasicOverload::Real => Some(NotEqReason::Real),
  }
}
