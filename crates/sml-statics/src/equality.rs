//! Checking if various structures respect/admit equality.

use crate::sym::{Equality, Sym};
use crate::types::{MetaTyVarKind, RecordTy, SubstEntry, Ty, TyScheme, TyVarKind};
use crate::{core_info::TyInfo, overload, st::St, ty_var::meta::Generalizable, util::instantiate};
use std::fmt;

pub(crate) type Result<T = (), E = NotEqTy> = std::result::Result<T, E>;

/// A type that is not equality.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum NotEqTy {
  FixedTyVar,
  Sym,
  Fn,
}

impl fmt::Display for NotEqTy {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      NotEqTy::FixedTyVar => f.write_str("a fixed non-equality type variable"),
      NotEqTy::Sym => f.write_str("a non-equality type constructor"),
      NotEqTy::Fn => f.write_str("a function type"),
    }
  }
}

fn all<I>(iter: I) -> Result
where
  I: Iterator<Item = Result>,
{
  for x in iter {
    x?;
  }
  Ok(())
}

/// Returns whether `ty` admits equality. That is:
///
/// - If it **is** an equality type, returns Ok.
/// - If it **is not** an equality type, returns Err with the first kind of type contained in it
///   that makes it not an equality type.
///
/// Also sets:
///
/// - Any non-constrained meta type variables to be equality type variables.
/// - Any overloaded meta type variables to be overloaded at a equality-only version of that
///   overload.
pub(crate) fn get_ty(st: &mut St, ty: &Ty) -> Result {
  if st.info.mode.is_path_order() {
    return Ok(());
  }
  match ty {
    Ty::None => Ok(()),
    Ty::BoundVar(_) => unreachable!("bound vars should be instantiated"),
    Ty::MetaVar(mv) => match st.subst.get(*mv) {
      None => {
        st.subst.insert(*mv, SubstEntry::Kind(TyVarKind::Equality.into()));
        Ok(())
      }
      Some(entry) => match entry.clone() {
        SubstEntry::Solved(ty) => get_ty(st, &ty),
        SubstEntry::Kind(kind) => match kind {
          MetaTyVarKind::TyVarKind(kind) => match kind {
            TyVarKind::Equality => Ok(()),
            TyVarKind::Overloaded(ov) => match ov {
              overload::Overload::Basic(basic) => get_basic(st, basic),
              overload::Overload::Composite(comp) => {
                let ov = equality_composite(comp);
                st.subst.insert(*mv, SubstEntry::Kind(TyVarKind::Overloaded(ov).into()));
                Ok(())
              }
            },
          },
          MetaTyVarKind::Record(rows, _) => get_record(st, &rows),
        },
      },
    },
    Ty::FixedVar(fv) => {
      if fv.ty_var().is_equality() {
        Ok(())
      } else {
        Err(NotEqTy::FixedTyVar)
      }
    }
    Ty::Record(rows) => get_record(st, rows),
    Ty::Con(args, sym) => get_con(st, args, *sym),
    Ty::Fn(_, _) => Err(NotEqTy::Fn),
  }
}

/// returns the "equality version" of this overload.
fn equality_composite(comp: overload::Composite) -> overload::Overload {
  match comp {
    overload::Composite::WordInt | overload::Composite::Num => overload::Composite::WordInt.into(),
    overload::Composite::RealInt => overload::Basic::Int.into(),
    overload::Composite::NumTxt | overload::Composite::WordIntTxt => {
      overload::Composite::WordIntTxt.into()
    }
  }
}

fn get_record(st: &mut St, rows: &RecordTy) -> Result {
  all(rows.values().map(|ty| get_ty(st, ty)))
}

fn get_basic(st: &mut St, ov: overload::Basic) -> Result {
  let ret = get_basic_opt(ov);
  // NOTE: this should always succeed because the signatures `INTEGER`, `WORD`, `STRING`, and `CHAR`
  // all have their primary types (e.g. `int` for `INTEGER`) as `eqtype`s.
  debug_assert_eq!(ret, get_basic_naive(st, ov));
  ret
}

/// optimized but ideally logically equivalent form of [`get_basic_naive`].
fn get_basic_opt(ov: overload::Basic) -> Result {
  match ov {
    overload::Basic::Int
    | overload::Basic::Word
    | overload::Basic::String
    | overload::Basic::Char => Ok(()),
    overload::Basic::Real => Err(NotEqTy::Sym),
  }
}

fn get_basic_naive(st: &mut St, ov: overload::Basic) -> Result {
  let syms = st.syms.overloads()[ov].clone();
  all(syms.into_iter().map(|sym| get_con(st, &[], sym)))
}

fn get_con(st: &mut St, args: &[Ty], sym: Sym) -> Result {
  let equality = match st.syms.get(sym) {
    Some(sym_info) => sym_info.equality,
    None => Equality::Never,
  };
  match equality {
    Equality::Always => Ok(()),
    Equality::Sometimes => all(args.iter().map(|ty| get_ty(st, ty))),
    Equality::Never => Err(NotEqTy::Sym),
  }
}

pub(crate) fn get_ty_scheme(st: &mut St, ty_scheme: TyScheme) -> Result {
  if st.info.mode.is_path_order() {
    return Ok(());
  }
  let ty = instantiate(st, Generalizable::Always, ty_scheme);
  get_ty(st, &ty)
}

pub(crate) fn get_ty_info(st: &mut St, ty_info: TyInfo) -> Result {
  if st.info.mode.is_path_order() {
    return Ok(());
  }
  get_ty_scheme(st, ty_info.ty_scheme)?;
  all(ty_info.val_env.into_iter().map(|(_, vi)| match vi.ty_scheme.ty {
    Ty::Fn(param, _) => {
      let param_ty_scheme = TyScheme { bound_vars: vi.ty_scheme.bound_vars, ty: *param };
      get_ty_scheme(st, param_ty_scheme)
    }
    _ => Ok(()),
  }))
}
