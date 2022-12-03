//! Checking if various structures respect/admit equality.

/// Whether equality type check are performed.
///
/// TODO remove this and all false branches.
pub const ENABLED: bool = true;

/// TODO remove this and all false branches. currently it fails, possible because of `where type`?
const ENABLE_DEBUG_CHECK: bool = false;

use crate::types::{
  BasicOverload, Equality, Generalizable, Overload, RecordTy, SubstEntry, Sym, Ty, TyInfo,
  TyScheme, TyVarKind,
};
use crate::{st::St, util::instantiate};
use std::fmt;

pub(crate) type Result<T = (), E = NotEqTy> = std::result::Result<T, E>;

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
/// - If it **is not** an equality type, returns Err with the first kind of type contained in it that
///   makes it not an equality type.
///
/// Also sets any non-constrained meta type variables to be equality type variables.
pub(crate) fn get_ty(st: &mut St, ty: &Ty) -> Result {
  if st.info.mode().is_path_order() || !ENABLED {
    return Ok(());
  }
  match ty {
    Ty::None => Ok(()),
    Ty::BoundVar(_) => unreachable!("bound vars should be instantiated"),
    Ty::MetaVar(mv) => match st.subst.get(*mv) {
      None => {
        st.subst.insert(*mv, SubstEntry::Kind(TyVarKind::Equality));
        Ok(())
      }
      Some(entry) => match entry.clone() {
        SubstEntry::Solved(ty) => get_ty(st, &ty),
        SubstEntry::Kind(kind) => match kind {
          TyVarKind::Equality => Ok(()),
          TyVarKind::Overloaded(ov) => match ov {
            Overload::Basic(basic) => get_basic(st, basic),
            Overload::Composite(comp) => {
              all(comp.as_basics().iter().map(|&basic| get_basic(st, basic)))
            }
          },
          TyVarKind::Record(rows) => get_record(st, &rows),
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

fn get_record(st: &mut St, rows: &RecordTy) -> Result {
  all(rows.values().map(|ty| get_ty(st, ty)))
}

fn get_basic(st: &mut St, ov: BasicOverload) -> Result {
  let ret = get_basic_opt(ov);
  // NOTE: this should always succeed because the signatures `INTEGER`, `WORD`, `STRING`, and `CHAR`
  // all have their primary types (e.g. `int` for `INTEGER`) as `eqtype`s.
  if ENABLE_DEBUG_CHECK {
    assert_eq!(ret, get_basic_naive(st, ov));
  }
  ret
}

/// optimized but ideally logically equivalent form of [`get_basic_naive`].
fn get_basic_opt(ov: BasicOverload) -> Result {
  match ov {
    BasicOverload::Int | BasicOverload::Word | BasicOverload::String | BasicOverload::Char => {
      Ok(())
    }
    BasicOverload::Real => Err(NotEqTy::Sym(Sym::REAL)),
  }
}

fn get_basic_naive(st: &mut St, ov: BasicOverload) -> Result {
  let syms = st.syms.overloads()[ov].clone();
  all(syms.into_iter().map(|sym| get_con(st, &[], sym)))
}

fn get_con(st: &mut St, args: &[Ty], sym: Sym) -> Result {
  match st.syms.equality(sym) {
    Equality::Always => Ok(()),
    Equality::Sometimes => all(args.iter().map(|ty| get_ty(st, ty))),
    Equality::Never => Err(NotEqTy::Sym(sym)),
  }
}

pub(crate) fn get_ty_scheme(st: &mut St, ty_scheme: TyScheme) -> Result {
  if st.info.mode().is_path_order() || !ENABLED {
    return Ok(());
  }
  let ty = instantiate(st, Generalizable::Always, ty_scheme);
  get_ty(st, &ty)
}

pub(crate) fn get_ty_info(st: &mut St, ty_info: TyInfo) -> Result {
  if st.info.mode().is_path_order() || !ENABLED {
    return Ok(());
  }
  let is_ref = match &ty_info.ty_scheme.ty {
    Ty::Con(args, sym) => args.is_empty() && *sym == Sym::REF,
    _ => false,
  };
  get_ty_scheme(st, ty_info.ty_scheme)?;
  if is_ref {
    Ok(())
  } else {
    all(ty_info.val_env.into_values().map(|vi| match vi.ty_scheme.ty {
      Ty::Fn(param, _) => {
        let param_ty_scheme = TyScheme { bound_vars: vi.ty_scheme.bound_vars, ty: *param };
        get_ty_scheme(st, param_ty_scheme)
      }
      _ => Ok(()),
    }))
  }
}
