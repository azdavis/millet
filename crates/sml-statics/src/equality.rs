//! Checking if a type is an equality type.

/// Whether equality type check are performed.
///
/// TODO remove this and all false branches.
pub const ENABLED: bool = true;

/// TODO remove this and all false branches. currently it fails, possible because of `where type`?
const ENABLE_DEBUG_CHECK: bool = false;

use crate::types::{
  BasicOverload, Equality, Generalizable, Overload, RecordTy, SubstEntry, Sym, Ty, TyScheme,
  TyVarKind,
};
use crate::{st::St, util::instantiate};
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Ans {
  Yes,
  No(NotEqTy),
}

impl Ans {
  fn all<I>(iter: I) -> Ans
  where
    I: Iterator<Item = Ans>,
  {
    for x in iter {
      match x {
        Ans::Yes => {}
        Ans::No(_) => return x,
      }
    }
    Ans::Yes
  }
}

/// A type that is not equality.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum NotEqTy {
  FixedTyVar,
  Sym(Sym),
  Fn,
}

impl fmt::Display for NotEqTy {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      NotEqTy::FixedTyVar => f.write_str("a fixed non-equality type variable"),
      NotEqTy::Sym(_) => f.write_str("a non-equality type constructor"),
      NotEqTy::Fn => f.write_str("a function type"),
    }
  }
}

/// Returns whether `ty` admits equality. That is:
///
/// - If it **is** an equality type, returns Yes.
/// - If it **is not** an equality type, returns No with the first kind of type contained in it that
///   makes it not an equality type.
///
/// Also sets any non-constrained meta type variables to be equality type variables.
pub(crate) fn get_ty(st: &mut St, ty: &Ty) -> Ans {
  if !ENABLED {
    return Ans::Yes;
  }
  match ty {
    Ty::None => Ans::Yes,
    Ty::BoundVar(_) => panic!("need binders to determine if bound var is equality"),
    Ty::MetaVar(mv) => match st.subst.get(*mv) {
      None => {
        st.subst.insert(*mv, SubstEntry::Kind(TyVarKind::Equality));
        Ans::Yes
      }
      Some(entry) => match entry.clone() {
        SubstEntry::Solved(ty) => get_ty(st, &ty),
        SubstEntry::Kind(kind) => match kind {
          TyVarKind::Equality => Ans::Yes,
          TyVarKind::Overloaded(ov) => match ov {
            Overload::Basic(basic) => get_basic(st, basic),
            Overload::Composite(comp) => {
              Ans::all(comp.as_basics().iter().map(|&basic| get_basic(st, basic)))
            }
          },
          TyVarKind::Record(rows) => get_record(st, &rows),
        },
      },
    },
    Ty::FixedVar(fv) => {
      if fv.ty_var().is_equality() {
        Ans::Yes
      } else {
        Ans::No(NotEqTy::FixedTyVar)
      }
    }
    Ty::Record(rows) => get_record(st, rows),
    Ty::Con(args, sym) => get_con(st, args, *sym),
    Ty::Fn(_, _) => Ans::No(NotEqTy::Fn),
  }
}

fn get_record(st: &mut St, rows: &RecordTy) -> Ans {
  Ans::all(rows.values().map(|ty| get_ty(st, ty)))
}

fn get_basic(st: &mut St, ov: BasicOverload) -> Ans {
  let ret = get_basic_opt(ov);
  // NOTE: this should always succeed because the signatures `INTEGER`, `WORD`, `STRING`, and `CHAR`
  // all have their primary types (e.g. `int` for `INTEGER`) as `eqtype`s.
  if ENABLE_DEBUG_CHECK {
    assert_eq!(ret, get_basic_naive(st, ov));
  }
  ret
}

/// optimized but ideally logically equivalent form of [`get_basic_naive`].
fn get_basic_opt(ov: BasicOverload) -> Ans {
  match ov {
    BasicOverload::Int | BasicOverload::Word | BasicOverload::String | BasicOverload::Char => {
      Ans::Yes
    }
    BasicOverload::Real => Ans::No(NotEqTy::Sym(Sym::REAL)),
  }
}

fn get_basic_naive(st: &mut St, ov: BasicOverload) -> Ans {
  let syms = st.syms.overloads()[ov].clone();
  Ans::all(syms.into_iter().map(|sym| get_con(st, &[], sym)))
}

fn get_con(st: &mut St, args: &[Ty], sym: Sym) -> Ans {
  match st.syms.equality(sym) {
    Equality::Always => Ans::Yes,
    Equality::Sometimes => Ans::all(args.iter().map(|ty| get_ty(st, ty))),
    Equality::Never => Ans::No(NotEqTy::Sym(sym)),
  }
}

#[allow(dead_code)]
pub(crate) fn get_ty_scheme(st: &mut St, ty_scheme: TyScheme) -> Ans {
  let ty = instantiate(st, Generalizable::Always, ty_scheme);
  get_ty(st, &ty)
}
