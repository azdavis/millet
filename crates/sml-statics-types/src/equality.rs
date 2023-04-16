//! Checking if various structures respect/admit equality.

use crate::info::TyInfo;
use crate::mode::Mode;
use crate::overload;
use crate::sym::{Equality, Sym, Syms};
use crate::ty::{
  Generalizable, RecordData, Ty, TyData, TyScheme, TyVarKind, Tys, UnsolvedMetaTyVarKind,
};
use crate::util::instantiate;
use std::fmt;

/// The result type for this module.
pub type Result<T = (), E = NotEqTy> = std::result::Result<T, E>;

/// A type that is not equality.
#[derive(Debug, PartialEq, Eq)]
pub enum NotEqTy {
  /// A fixed (non-equality) type variable.
  FixedTyVar,
  /// A symbol.
  Sym,
  /// A function.
  Fn,
}

impl fmt::Display for NotEqTy {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::FixedTyVar => f.write_str("a fixed non-equality type variable"),
      Self::Sym => f.write_str("a non-equality type constructor"),
      Self::Fn => f.write_str("a function type"),
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

/// Returns whether `ty` admits equality.
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
pub(crate) fn get_ty(mode: Mode, syms: &Syms, tys: &mut Tys, ty: Ty) -> Result {
  if mode.is_path_order() {
    return Ok(());
  }
  match tys.data(ty) {
    TyData::None => Ok(()),
    TyData::BoundVar(_) => unreachable!("bound vars should be instantiated"),
    TyData::UnsolvedMetaVar(umv) => match umv.kind {
      UnsolvedMetaTyVarKind::Kind(kind) => match kind {
        TyVarKind::Regular => {
          tys.unsolved_meta_var(ty).kind = UnsolvedMetaTyVarKind::Kind(TyVarKind::Equality);
          Ok(())
        }
        TyVarKind::Equality => Ok(()),
        TyVarKind::Overloaded(ov) => match ov {
          overload::Overload::Basic(basic) => get_basic(mode, syms, tys, basic),
          overload::Overload::Composite(comp) => {
            let ov = equality_composite(comp);
            tys.unsolved_meta_var(ty).kind = UnsolvedMetaTyVarKind::Kind(TyVarKind::Overloaded(ov));
            Ok(())
          }
        },
      },
      UnsolvedMetaTyVarKind::UnresolvedRecord(ur) => get_record(mode, syms, tys, &ur.rows),
    },
    TyData::FixedVar(fv) => {
      if fv.ty_var.is_equality() {
        Ok(())
      } else {
        Err(NotEqTy::FixedTyVar)
      }
    }
    TyData::Record(rows) => get_record(mode, syms, tys, &rows),
    TyData::Con(data) => get_con(mode, syms, tys, &data.args, data.sym),
    TyData::Fn(_) => Err(NotEqTy::Fn),
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

fn get_record(mode: Mode, syms: &Syms, tys: &mut Tys, rows: &RecordData) -> Result {
  all(rows.values().map(|&ty| get_ty(mode, syms, tys, ty)))
}

fn get_basic(mode: Mode, syms: &Syms, tys: &mut Tys, ov: overload::Basic) -> Result {
  let ret = get_basic_opt(ov);
  // NOTE: this should always succeed because the signatures `INTEGER`, `WORD`, `STRING`, and `CHAR`
  // all have their primary types (e.g. `int` for `INTEGER`) as `eqtype`s.
  debug_assert_eq!(ret, get_basic_naive(mode, syms, tys, ov));
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

fn get_basic_naive(mode: Mode, syms: &Syms, tys: &mut Tys, ov: overload::Basic) -> Result {
  let ov_syms = syms.overloads()[ov].clone();
  all(ov_syms.into_iter().map(|sym| get_con(mode, syms, tys, &[], sym)))
}

fn get_con(mode: Mode, syms: &Syms, tys: &mut Tys, args: &[Ty], sym: Sym) -> Result {
  let equality = match syms.get(sym) {
    Some(sym_info) => sym_info.equality,
    None => Equality::Never,
  };
  match equality {
    Equality::Always => Ok(()),
    Equality::Sometimes => all(args.iter().map(|&ty| get_ty(mode, syms, tys, ty))),
    Equality::Never => Err(NotEqTy::Sym),
  }
}

/// Checks the ty scheme.
///
/// # Errors
///
/// If it doesn't respect equality.
pub fn get_ty_scheme(mode: Mode, syms: &Syms, tys: &mut Tys, ty_scheme: &TyScheme) -> Result {
  if mode.is_path_order() {
    return Ok(());
  }
  let ty = instantiate(tys, Generalizable::Always, ty_scheme);
  get_ty(mode, syms, tys, ty)
}

/// Checks the ty info.
///
/// # Errors
///
/// If it doesn't respect equality.
pub fn get_ty_info(mode: Mode, syms: &Syms, tys: &mut Tys, ty_info: TyInfo) -> Result {
  if mode.is_path_order() {
    return Ok(());
  }
  get_ty_scheme(mode, syms, tys, &ty_info.ty_scheme)?;
  all(ty_info.val_env.into_iter().map(|(_, vi)| match tys.data(vi.ty_scheme.ty) {
    TyData::Fn(data) => {
      let param_ty_scheme = TyScheme { bound_vars: vi.ty_scheme.bound_vars, ty: data.param };
      get_ty_scheme(mode, syms, tys, &param_ty_scheme)
    }
    _ => Ok(()),
  }))
}
