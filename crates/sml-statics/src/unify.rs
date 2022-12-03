//! Unification: given two types, figuring out whether they are "compatible", and if so, how.

use crate::equality;
use crate::error::{ErrorKind, MismatchedTypesFlavor};
use crate::st::St;
use crate::types::{MetaTyVar, SubstEntry, Ty, TyVarKind};
use crate::util::{apply, meta_vars};
use fast_hash::FxHashMap;

pub(crate) fn unify(st: &mut St, want: Ty, got: Ty, idx: sml_hir::Idx) {
  match unify_no_emit(st, want, got) {
    Ok(()) => {}
    Err(e) => st.err(idx, e),
  }
}

pub(crate) fn unify_no_emit(st: &mut St, want: Ty, got: Ty) -> Result<(), ErrorKind> {
  if st.info.mode().is_path_order() {
    return Ok(());
  }
  unify_(st, want.clone(), got.clone()).map_err(|err| match err {
    UnifyError::Circularity(mv, ty) => ErrorKind::Circularity(mv, ty),
    UnifyError::MismatchedTypes(flavor) => ErrorKind::MismatchedTypes(flavor, want, got),
  })
}

#[derive(Debug)]
enum UnifyError {
  Circularity(MetaTyVar, Ty),
  MismatchedTypes(MismatchedTypesFlavor),
}

impl From<MismatchedTypesFlavor> for UnifyError {
  fn from(val: MismatchedTypesFlavor) -> Self {
    Self::MismatchedTypes(val)
  }
}

/// does not emit any errors to the `st`, instead returns an error (if any).
///
/// `want` and `got` will have `subst` applied to them upon entry to this function.
fn unify_(st: &mut St, mut want: Ty, mut got: Ty) -> Result<(), UnifyError> {
  apply(&st.subst, &mut want);
  apply(&st.subst, &mut got);
  match (want, got) {
    (Ty::None, _) | (_, Ty::None) => Ok(()),
    (Ty::BoundVar(want), Ty::BoundVar(got)) => {
      if want == got {
        Ok(())
      } else {
        Err(MismatchedTypesFlavor::BoundTyVar(want, got).into())
      }
    }
    (Ty::MetaVar(mv), ty) | (ty, Ty::MetaVar(mv)) => unify_mv(st, mv, ty),
    (Ty::FixedVar(want), Ty::FixedVar(got)) => {
      if want == got {
        Ok(())
      } else {
        Err(MismatchedTypesFlavor::FixedTyVar(want, got).into())
      }
    }
    (Ty::Record(want_rows), Ty::Record(mut got_rows)) => {
      for (lab, want) in want_rows {
        match got_rows.remove(&lab) {
          None => return Err(MismatchedTypesFlavor::MissingRow(lab).into()),
          Some(got) => unify_(st, want, got)?,
        }
      }
      if got_rows.is_empty() {
        Ok(())
      } else {
        Err(MismatchedTypesFlavor::ExtraRows(got_rows).into())
      }
    }
    (Ty::Con(want_args, want_sym), Ty::Con(got_args, got_sym)) => {
      if want_sym != got_sym {
        return Err(MismatchedTypesFlavor::Con(want_sym, got_sym).into());
      }
      assert_eq!(want_args.len(), got_args.len());
      for (want, got) in want_args.into_iter().zip(got_args) {
        unify_(st, want, got)?;
      }
      Ok(())
    }
    (Ty::Fn(want_param, want_res), Ty::Fn(got_param, got_res)) => {
      unify_(st, *want_param, *got_param)?;
      unify_(st, *want_res, *got_res)
    }
    (want, got) => Err(MismatchedTypesFlavor::Head(want, got).into()),
  }
}

fn unify_mv(st: &mut St, mv: MetaTyVar, mut ty: Ty) -> Result<(), UnifyError> {
  // return without doing anything if the meta vars are the same.
  if let Ty::MetaVar(mv2) = &ty {
    if mv == *mv2 {
      return Ok(());
    }
  }
  // forbid circularity.
  if occurs(mv, &ty) {
    return Err(UnifyError::Circularity(mv, ty));
  }
  // tweak down the rank of all other meta vars in the ty.
  let mut map = FxHashMap::<MetaTyVar, (MetaTyVar, Option<TyVarKind>)>::default();
  meta_vars(
    &st.subst,
    &mut |x, k| {
      // this is crucial!
      if mv.rank_lt(x) {
        map.insert(x, (st.meta_gen.gen_same_rank(mv), k.cloned()));
      }
    },
    &ty,
  );
  for (k, (v, kind)) in map {
    st.subst.insert(k, SubstEntry::Solved(Ty::MetaVar(v)));
    if let Some(kind) = kind {
      st.subst.insert(v, SubstEntry::Kind(kind));
    }
  }
  apply(&st.subst, &mut ty);
  // solve mv to ty. however, mv may already have an entry.
  match st.subst.insert(mv, SubstEntry::Solved(ty.clone())) {
    // do nothing if no entry.
    None => {}
    // unreachable because we applied upon entry.
    Some(SubstEntry::Solved(ty)) => unreachable!("meta var already solved to {ty:?}"),
    Some(SubstEntry::Kind(kind)) => match kind {
      TyVarKind::Equality => match equality::get_ty(&mut st.subst, &ty) {
        equality::Ans::Yes => {}
        equality::Ans::No(not_eq) => {
          return Err(MismatchedTypesFlavor::NotEqTy(ty.clone(), not_eq).into())
        }
      },
      // mv was an overloaded ty var. ty must conform to that overload.
      TyVarKind::Overloaded(ov) => match ty {
        // don't emit more errors for None.
        Ty::None => {}
        // the simple case. check the sym is in the overload.
        Ty::Con(args, s) => {
          if ov.as_basics().iter().any(|&ov| st.syms.overloads()[ov].contains(&s)) {
            assert!(args.is_empty());
          } else {
            return Err(MismatchedTypesFlavor::OverloadCon(ov, s).into());
          }
        }
        // we solved mv = mv2. now we give mv2 mv's old entry, to make it an overloaded ty var.
        // but mv2 itself may also have an entry.
        Ty::MetaVar(mv2) => {
          let ov = match &mut st.subst.get(mv2) {
            // it didn't have an entry.
            None => ov,
            // unreachable because of apply.
            Some(SubstEntry::Solved(ty)) => unreachable!("meta var already solved to {ty:?}"),
            Some(SubstEntry::Kind(kind)) => match kind {
              TyVarKind::Equality => match equality::get_ty(&mut st.subst, &Ty::MetaVar(mv)) {
                equality::Ans::Yes => ov,
                equality::Ans::No(not_eq) => {
                  return Err(MismatchedTypesFlavor::NotEqTy(Ty::MetaVar(mv), not_eq).into())
                }
              },
              // it too was an overload. attempt to unify the two overloads.
              TyVarKind::Overloaded(ov_other) => match ov.unify(*ov_other) {
                Some(ov) => ov,
                None => return Err(MismatchedTypesFlavor::OverloadUnify(ov, *ov_other).into()),
              },
              // no overloaded type is a record.
              TyVarKind::Record(rows) => {
                return Err(MismatchedTypesFlavor::OverloadRecord(rows.clone(), ov).into())
              }
            },
          };
          let k = SubstEntry::Kind(TyVarKind::Overloaded(ov));
          st.subst.insert(mv2, k);
        }
        // none of these are overloaded types.
        Ty::BoundVar(_) | Ty::FixedVar(_) | Ty::Record(_) | Ty::Fn(_, _) => {
          return Err(MismatchedTypesFlavor::OverloadHeadMismatch(ov, ty).into())
        }
      },
      // mv was a record ty var (i.e. from a `...` pattern). ty must have the rows gotten so
      // far.
      TyVarKind::Record(mut want_rows) => match ty {
        // don't emit more errors for None.
        Ty::None => {}
        // ty was a record. it should have every label in the wanted rows, and the types should
        // unify.
        Ty::Record(mut got_rows) => {
          for (lab, want) in want_rows {
            match got_rows.remove(&lab) {
              None => return Err(MismatchedTypesFlavor::UnresolvedRecordMissingRow(lab).into()),
              Some(got) => unify_(st, want, got)?,
            }
          }
        }
        // ty was a meta var, so we solved mv = mv2. check if mv2 has its own entry before
        // setting mv2's entry to mv's old entry, which specifies the rows for this record ty
        // var.
        Ty::MetaVar(mv2) => {
          match &mut st.subst.get(mv2) {
            // there was no entry.
            None => {}
            // unreachable because of apply.
            Some(SubstEntry::Solved(ty)) => unreachable!("meta var already solved to {ty:?}"),
            Some(SubstEntry::Kind(kind)) => match kind {
              TyVarKind::Equality => match equality::get_ty(&mut st.subst, &Ty::MetaVar(mv)) {
                equality::Ans::Yes => {}
                equality::Ans::No(not_eq) => {
                  return Err(MismatchedTypesFlavor::NotEqTy(Ty::MetaVar(mv), not_eq).into())
                }
              },
              // no overloaded type is a record type.
              TyVarKind::Overloaded(ov) => {
                return Err(MismatchedTypesFlavor::OverloadRecord(want_rows, *ov).into())
              }
              // mv2 was another record ty var. merge the rows, and for those that appear in
              // both, unify the types.
              TyVarKind::Record(other_rows) => {
                for (lab, mut want) in other_rows.clone() {
                  if let Some(got) = want_rows.get(&lab) {
                    unify_(st, want.clone(), got.clone())?;
                    apply(&st.subst, &mut want);
                  }
                  want_rows.insert(lab, want);
                }
              }
            },
          }
          // set the entry to make mv2 a record ty var.
          let k = SubstEntry::Kind(TyVarKind::Record(want_rows));
          st.subst.insert(mv2, k);
        }
        // none of these are record types.
        Ty::BoundVar(_) | Ty::FixedVar(_) | Ty::Con(_, _) | Ty::Fn(_, _) => {
          return Err(MismatchedTypesFlavor::UnresolvedRecordHeadMismatch(want_rows, ty).into())
        }
      },
    },
  }
  Ok(())
}

fn occurs(mv: MetaTyVar, ty: &Ty) -> bool {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::FixedVar(_) => false,
    Ty::MetaVar(mv2) => mv == *mv2,
    Ty::Record(rows) => rows.values().any(|t| occurs(mv, t)),
    Ty::Con(args, _) => args.iter().any(|t| occurs(mv, t)),
    Ty::Fn(param, res) => occurs(mv, param) || occurs(mv, res),
  }
}
