//! Unification: given two types, figuring out whether they are "compatible", and if so, how.

use crate::sym::{Sym, Syms};
use crate::ty::{
  MetaTyVarData, RecordData, Ty, TyData, TyKind, TyVarKind, Tys, UnsolvedMetaTyVarData,
  UnsolvedMetaTyVarKind,
};
use crate::{equality, overload};
use std::collections::BTreeSet;

/// An error when unifying.
#[derive(Debug)]
pub enum Error {
  /// A circularity error.
  Circularity(Circularity),
  /// A incompatible error.
  Incompatible(Incompatible),
}

/// A type was circular.
#[derive(Debug)]
pub struct Circularity {
  /// This ty, a meta var, appears in `ty`.
  pub meta_var: Ty,
  /// Contains `meta_var`, but is not equal to `meta_var`.
  pub ty: Ty,
}

/// A reason why types were incompatible.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Incompatible {
  FixedTyVar(sml_hir::TyVar, sml_hir::TyVar),
  MismatchedRows { missing: BTreeSet<sml_hir::Lab>, extra: BTreeSet<sml_hir::Lab> },
  Con(Sym, Sym),
  HeadMismatch(Ty, Ty),
  OverloadCon(overload::Overload, Sym),
  OverloadUnify(overload::Overload, overload::Overload),
  OverloadRecord(overload::Overload, RecordData),
  OverloadHeadMismatch(overload::Overload, Ty),
  UnresolvedRecordMissingRow(sml_hir::Lab),
  UnresolvedRecordHeadMismatch(RecordData, Ty),
  NotEqTy(Ty, equality::NotEqTy),
}

impl From<Incompatible> for Error {
  fn from(val: Incompatible) -> Self {
    Self::Incompatible(val)
  }
}

/// Unifies two types, updating `tys` as necessary to record how.
///
/// # Errors
///
/// If the types couldn't be unified.
///
/// # Panics
///
/// If the types contain bound variables.
pub fn unify(tys: &mut Tys, syms: &Syms, want: Ty, got: Ty) -> Result<(), Error> {
  let (want, want_data) = tys.canonicalize(want);
  let (got, got_data) = tys.canonicalize(got);
  // if `Ty`s are `==`, they are semantically the same type, because of interning.
  if want == got {
    return Ok(());
  }
  match (want_data, got_data) {
    (TyData::None, _) | (_, TyData::None) => Ok(()),
    (TyData::BoundVar(_), _) | (_, TyData::BoundVar(_)) => {
      unreachable!("bound vars should be instantiated")
    }
    (TyData::UnsolvedMetaVar(umv), _) => unify_mv(tys, syms, want, umv, got),
    (_, TyData::UnsolvedMetaVar(umv)) => unify_mv(tys, syms, got, umv, want),
    (TyData::FixedVar(want), TyData::FixedVar(got)) => {
      // already checked not equal
      Err(Incompatible::FixedTyVar(want.ty_var, got.ty_var).into())
    }
    (TyData::Record(want_rows), TyData::Record(mut got_rows)) => {
      let mut missing = BTreeSet::<sml_hir::Lab>::default();
      for (lab, want_ty) in want_rows {
        match got_rows.remove(&lab) {
          None => {
            missing.insert(lab);
          }
          Some(got_ty) => unify(tys, syms, want_ty, got_ty)?,
        }
      }
      if got_rows.is_empty() && missing.is_empty() {
        Ok(())
      } else {
        let extra: BTreeSet<_> = got_rows.into_keys().collect();
        Err(Incompatible::MismatchedRows { missing, extra }.into())
      }
    }
    (TyData::Con(want), TyData::Con(got)) => {
      if want.sym != got.sym {
        return Err(Incompatible::Con(want.sym, got.sym).into());
      }
      assert_eq!(want.args.len(), got.args.len());
      for (&want, &got) in want.args.iter().zip(got.args.iter()) {
        unify(tys, syms, want, got)?;
      }
      Ok(())
    }
    (TyData::Fn(want), TyData::Fn(got)) => {
      unify(tys, syms, want.param, got.param)?;
      unify(tys, syms, want.res, got.res)
    }
    _ => Err(Incompatible::HeadMismatch(want, got).into()),
  }
}

/// unifies mv, which is currently unsolved and has data `mv_data`, which describes the
/// restrictions if any on it, with ty.
fn unify_mv(
  tys: &mut Tys,
  syms: &Syms,
  mv: Ty,
  mv_data: UnsolvedMetaTyVarData,
  ty: Ty,
) -> Result<(), Error> {
  debug_assert!(matches!(tys.data(mv), TyData::UnsolvedMetaVar(_)));
  debug_assert_eq!(tys.canonicalize(mv).0, mv);
  debug_assert_eq!(tys.canonicalize(ty).0, ty);
  // make sure we're trying to solve the right ty.
  let ty = {
    let mut tmp = ty;
    while let TyKind::MetaVar = ty.kind {
      match &tys.meta_var_data[ty.idx.to_usize()] {
        MetaTyVarData::Solved(new_ty) => tmp = *new_ty,
        MetaTyVarData::Unsolved(_) => break,
      }
    }
    tmp
  };
  // allow solving to itself as a no-op.
  if mv == ty {
    return Ok(());
  }
  // adjust the ranks for meta vars in ty, and also fail if the occurs check fails.
  match adjust_mv_ranks(tys, mv, &mv_data, ty) {
    Ok(()) => {}
    Err(()) => return Err(Error::Circularity(Circularity { meta_var: mv, ty })),
  }
  // check that ty is compatible with what that meta var currently is.
  match mv_data.kind {
    UnsolvedMetaTyVarKind::Kind(kind) => match kind {
      // mv is a regular meta var, allowing all types.
      TyVarKind::Regular => {}
      // mv is an equality meta var. ty must be an equality ty as well.
      TyVarKind::Equality => match equality::get_ty(syms, tys, ty) {
        Ok(()) => {}
        Err(e) => return Err(Incompatible::NotEqTy(ty, e).into()),
      },
      // mv is an overloaded meta var. ty must be compatible with the overload.
      TyVarKind::Overloaded(ov) => match tys.data(ty) {
        // ignore.
        TyData::None => {}
        // none of these are overloaded types.
        TyData::BoundVar(_) | TyData::FixedVar(_) | TyData::Record(_) | TyData::Fn(_) => {
          return Err(Incompatible::OverloadHeadMismatch(ov, ty).into());
        }
        // ty is a con. the con must be compatible with the overload.
        TyData::Con(data) => {
          if ov.as_basics().iter().any(|&ov| syms.overloads()[ov].contains(&data.sym)) {
            assert!(data.args.is_empty(), "overloaded syms do not have ty args");
          } else {
            return Err(Incompatible::OverloadCon(ov, data.sym).into());
          }
        }
        // ty is an unsolved meta var. it must now be an overloaded meta var.
        TyData::UnsolvedMetaVar(ty_unsolved) => {
          let ty_ov = match ty_unsolved.kind {
            UnsolvedMetaTyVarKind::Kind(kind) => match kind {
              // ty is a regular meta var, allowing all types.
              TyVarKind::Regular => ov,
              // ty is an equality meta var. mv must be an equality overload.
              TyVarKind::Equality => match equality::get_ty(syms, tys, mv) {
                Ok(()) => ov,
                Err(e) => return Err(Incompatible::NotEqTy(mv, e).into()),
              },
              // ty is an overloaded meta var as well. the overloads must be compatible.
              TyVarKind::Overloaded(ty_ov) => match ov.unify(ty_ov) {
                Some(ty_ov) => ty_ov,
                None => return Err(Incompatible::OverloadUnify(ov, ty_ov).into()),
              },
            },
            // ty is an unresolved record. no overload is an unresolved record.
            UnsolvedMetaTyVarKind::UnresolvedRecord(ur) => {
              return Err(Incompatible::OverloadRecord(ov, ur.rows).into());
            }
          };
          // make ty an overloaded meta var.
          tys.unsolved_meta_var(ty).kind =
            UnsolvedMetaTyVarKind::Kind(TyVarKind::Overloaded(ty_ov));
        }
      },
    },
    // mv is an unresolved record ty var (i.e. from a `...` pattern). ty must have the rows gotten
    // so far.
    UnsolvedMetaTyVarKind::UnresolvedRecord(mut want) => match tys.data(ty) {
      // ignore.
      TyData::None => {}
      // none of these are record types.
      TyData::BoundVar(_) | TyData::FixedVar(_) | TyData::Con(_) | TyData::Fn(_) => {
        return Err(Incompatible::UnresolvedRecordHeadMismatch(want.rows, ty).into());
      }
      // ty is a record. it should have every label in the currently resolved rows, and the
      // corresponding types for those rows should unify.
      TyData::Record(mut got_rows) => {
        for (lab, want) in want.rows {
          match got_rows.remove(&lab) {
            None => return Err(Incompatible::UnresolvedRecordMissingRow(lab).into()),
            Some(got) => unify(tys, syms, want, got)?,
          }
        }
      }
      // ty is an unsolved meta var as well. it must now be a unresolved record meta var.
      TyData::UnsolvedMetaVar(ty_unsolved) => {
        match ty_unsolved.kind {
          UnsolvedMetaTyVarKind::Kind(kind) => match kind {
            // ty is a regular meta var, allowing all types.
            TyVarKind::Regular => {}
            // ty is an equality meta var. the types in the rows so far must be all equality types
            // as well. mv contains those rows.
            TyVarKind::Equality => match equality::get_ty(syms, tys, mv) {
              Ok(()) => {}
              Err(e) => return Err(Incompatible::NotEqTy(mv, e).into()),
            },
            // ty is an overloaded meta var. no overload is a record type.
            TyVarKind::Overloaded(ov) => {
              return Err(Incompatible::OverloadRecord(ov, want.rows).into());
            }
          },
          // ty is an unresolved record meta var as well. merge the rows, and for those rows that
          // appeared in both, unify the types.
          UnsolvedMetaTyVarKind::UnresolvedRecord(got) => {
            for (lab, got) in got.rows {
              if let Some(&want) = want.rows.get(&lab) {
                unify(tys, syms, want, got)?;
              }
              want.rows.insert(lab, got);
            }
          }
        }
        tys.unsolved_meta_var(ty).kind = UnsolvedMetaTyVarKind::UnresolvedRecord(want);
      }
    },
  }
  // solve mv to ty. mv is already known to be canonically an unsolved meta var.
  tys.meta_var_data[mv.idx.to_usize()] = MetaTyVarData::Solved(ty);
  Ok(())
}

/// adjust the ranks of all unsolved meta vars in `ty` to be no higher than the rank of the meta var
/// with the given unsolved data, which is about to be solved to `ty`. also check this meta var does
/// not appear in `ty` (the "occurs check").
fn adjust_mv_ranks(
  tys: &mut Tys,
  mv: Ty,
  mv_data: &UnsolvedMetaTyVarData,
  ty: Ty,
) -> Result<(), ()> {
  let (ty, data) = tys.canonicalize(ty);
  match data {
    // the interesting case
    TyData::UnsolvedMetaVar(ty_mv_unsolved) => {
      if mv == ty {
        return Err(());
      }
      if mv_data.rank < ty_mv_unsolved.rank {
        tys.unsolved_meta_var(ty).rank = mv_data.rank;
      }
      Ok(())
    }
    // no-op base cases
    TyData::None | TyData::BoundVar(_) | TyData::FixedVar(_) => Ok(()),
    // recursive cases
    TyData::Record(rows) => {
      for (_, ty) in rows {
        adjust_mv_ranks(tys, mv, mv_data, ty)?;
      }
      Ok(())
    }
    TyData::Con(data) => {
      for ty in data.args {
        adjust_mv_ranks(tys, mv, mv_data, ty)?;
      }
      Ok(())
    }
    TyData::Fn(data) => {
      adjust_mv_ranks(tys, mv, mv_data, data.param)?;
      adjust_mv_ranks(tys, mv, mv_data, data.res)?;
      Ok(())
    }
  }
}
