//! Checking if a type is an equality type. TODO use

#![allow(dead_code)]

use crate::types::{
  BasicOverload, Overload, RecordTy, Subst, SubstEntry, Sym, Syms, Ty, TyVarKind,
};

/// Returns true iff this is an equality type.
pub(crate) fn ck(syms: &Syms, subst: &Subst, ty: &Ty) -> bool {
  match ty {
    Ty::None | Ty::Fn(_, _) => false,
    Ty::BoundVar(_) => panic!("need binders to determine if bound var is equality"),
    Ty::MetaVar(mv) => match subst.get(*mv) {
      None => false,
      Some(entry) => match entry {
        SubstEntry::Solved(ty) => ck(syms, subst, ty),
        SubstEntry::Kind(kind) => match kind {
          TyVarKind::Equality => true,
          TyVarKind::Overloaded(ov) => match ov {
            Overload::Basic(basic) => ck_basic_overload(syms, *basic),
            Overload::Composite(comp) => {
              comp.as_basics().iter().all(|&basic| ck_basic_overload(syms, basic))
            }
          },
          TyVarKind::Record(rows) => ck_record(syms, subst, rows),
        },
      },
    },
    Ty::FixedVar(fv) => fv.ty_var().is_equality(),
    Ty::Record(rows) => ck_record(syms, subst, rows),
    Ty::Con(args, sym) => {
      // TODO arrays should be equality?
      if *sym == Sym::REAL {
        false
      } else if *sym == Sym::REF {
        true
      } else {
        args.iter().all(|ty| ck(syms, subst, ty))
      }
    }
  }
}

fn ck_record(syms: &Syms, subst: &Subst, rows: &RecordTy) -> bool {
  rows.values().all(|ty| ck(syms, subst, ty))
}

fn ck_basic_overload(syms: &Syms, ov: BasicOverload) -> bool {
  syms.overloads()[ov].iter().all(|&sym| sym != Sym::REAL)
}
