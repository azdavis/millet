//! Checking if a type is an equality type.

/// Whether equality type check are performed.
///
/// TODO remove this and all false branches.
pub const ENABLED: bool = false;

use crate::types::{BasicOverload, Overload, RecordTy, Subst, SubstEntry, Sym, Ty, TyVarKind};
use std::fmt;

/// A type that is not equality.
#[derive(Debug)]
pub(crate) enum NotEqTy {
  FixedTyVar,
  Real,
  Fn,
}

impl fmt::Display for NotEqTy {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      NotEqTy::FixedTyVar => f.write_str("a fixed non-equality type variable"),
      NotEqTy::Real => f.write_str("`real`"),
      NotEqTy::Fn => f.write_str("a function type"),
    }
  }
}

/// Returns a witness to the given type being **not** an equality type, if there is one. That is:
///
/// - If it **is** an equality type, returns nothing.
/// - If it **is not** an equality type, returns the first kind of type contained in it that makes
///   it not an equality type.
///
/// Also sets any non-constrained meta type variables to be equality type variables.
pub(crate) fn get(subst: &mut Subst, ty: &Ty) -> Option<NotEqTy> {
  if !ENABLED {
    return None;
  }
  match ty {
    Ty::None => None,
    Ty::BoundVar(_) => panic!("need binders to determine if bound var is equality"),
    Ty::MetaVar(mv) => match subst.get(*mv) {
      None => {
        subst.insert(*mv, SubstEntry::Kind(TyVarKind::Equality));
        None
      }
      Some(entry) => match entry.clone() {
        SubstEntry::Solved(ty) => get(subst, &ty),
        SubstEntry::Kind(kind) => match kind {
          TyVarKind::Equality => None,
          TyVarKind::Overloaded(ov) => match ov {
            Overload::Basic(basic) => get_basic(basic),
            Overload::Composite(comp) => {
              comp.as_basics().iter().find_map(|&basic| get_basic(basic))
            }
          },
          TyVarKind::Record(rows) => get_record(subst, &rows),
        },
      },
    },
    Ty::FixedVar(fv) => (!fv.ty_var().is_equality()).then_some(NotEqTy::FixedTyVar),
    Ty::Record(rows) => get_record(subst, rows),
    Ty::Con(args, sym) => {
      // TODO arrays should be equality?
      if *sym == Sym::REAL {
        Some(NotEqTy::Real)
      } else if *sym == Sym::REF {
        None
      } else {
        args.iter().find_map(|ty| get(subst, ty))
      }
    }
    Ty::Fn(_, _) => Some(NotEqTy::Fn),
  }
}

fn get_record(subst: &mut Subst, rows: &RecordTy) -> Option<NotEqTy> {
  rows.values().find_map(|ty| get(subst, ty))
}

/// NOTE: this is an optimization. The (ideally, if our assumptions are correct) equivalent but
/// slower thing to do would be iterate over all the real symbols for the basic overload and check
/// whether all of them are equality.
///
/// However, that should always return the same result as this because the signatures `INTEGER`,
/// `WORD`, `STRING`, and `CHAR` all have their primary types (e.g. `int` for `INTEGER`) as
/// `eqtype`s.
fn get_basic(ov: BasicOverload) -> Option<NotEqTy> {
  match ov {
    BasicOverload::Int | BasicOverload::Word | BasicOverload::String | BasicOverload::Char => None,
    BasicOverload::Real => Some(NotEqTy::Real),
  }
}
