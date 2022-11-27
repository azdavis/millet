//! Checking if a type is an equality type. TODO use

#![allow(dead_code)]

use crate::types::{BasicOverload, Overload, RecordTy, Subst, SubstEntry, Sym, Ty, TyVarKind};

/// Returns true iff this is an equality type.
pub(crate) fn ck(subst: &Subst, ty: &Ty) -> bool {
  match ty {
    Ty::None | Ty::Fn(_, _) => false,
    Ty::BoundVar(_) => panic!("need binders to determine if bound var is equality"),
    Ty::MetaVar(mv) => match subst.get(*mv) {
      None => true,
      Some(entry) => match entry {
        SubstEntry::Solved(ty) => ck(subst, ty),
        SubstEntry::Kind(kind) => match kind {
          TyVarKind::Equality => true,
          TyVarKind::Overloaded(ov) => match ov {
            Overload::Basic(basic) => ck_basic_overload(*basic),
            Overload::Composite(comp) => {
              comp.as_basics().iter().all(|&basic| ck_basic_overload(basic))
            }
          },
          TyVarKind::Record(rows) => ck_record(subst, rows),
        },
      },
    },
    Ty::FixedVar(fv) => fv.ty_var().is_equality(),
    Ty::Record(rows) => ck_record(subst, rows),
    Ty::Con(args, sym) => {
      // TODO arrays should be equality?
      if *sym == Sym::REAL {
        false
      } else if *sym == Sym::REF {
        true
      } else {
        args.iter().all(|ty| ck(subst, ty))
      }
    }
  }
}

fn ck_record(subst: &Subst, rows: &RecordTy) -> bool {
  rows.values().all(|ty| ck(subst, ty))
}

/// NOTE: this is an optimization. The (ideally, if our assumptions are correct) equivalent but
/// slower thing to do would be iterate over all the real symbols for the basic overload and check
/// whether all of them are equality.
///
/// However, that should always return the same result as this because the signatures `INTEGER`,
/// `WORD`, `STRING`, and `CHAR` all have their primary types (e.g. `int` for `INTEGER`) as
/// `eqtype`s.
fn ck_basic_overload(ov: BasicOverload) -> bool {
  match ov {
    BasicOverload::Int | BasicOverload::Word | BasicOverload::String | BasicOverload::Char => true,
    BasicOverload::Real => false,
  }
}
