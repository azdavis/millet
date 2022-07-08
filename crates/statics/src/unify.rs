use crate::error::ErrorKind;
use crate::st::St;
use crate::types::{MetaTyVar, Overload, Overloads, Subst, SubstEntry, Ty, TyVarKind};
use crate::util::apply;

#[derive(Debug)]
enum UnifyError {
  OccursCheck(MetaTyVar, Ty),
  HeadMismatch,
  OverloadMismatch(Overload, MetaTyVar),
}

type Result<T, E = UnifyError> = std::result::Result<T, E>;

pub(crate) fn unify<I>(st: &mut St, want: Ty, got: Ty, idx: I)
where
  I: Into<hir::Idx>,
{
  let mut unify_st = UnifySt {
    overloads: std::mem::take(st.syms.overloads()),
    subst: std::mem::take(st.subst()),
  };
  match unify_(&mut unify_st, want.clone(), got.clone()) {
    Ok(()) => {}
    Err(e) => {
      let e = match e {
        UnifyError::OccursCheck(mv, ty) => ErrorKind::Circularity(mv, ty),
        UnifyError::HeadMismatch => ErrorKind::MismatchedTypes(want, got),
        UnifyError::OverloadMismatch(ov, mv) => ErrorKind::OverloadMismatch(ov, mv, want, got),
      };
      st.err(idx, e);
    }
  };
  *st.syms.overloads() = unify_st.overloads;
  *st.subst() = unify_st.subst;
}

/// use this to avoid taking the whole [`St`] in [`unify_`].
struct UnifySt {
  overloads: Overloads,
  subst: Subst,
}

/// `want` and `got` will have `subst` applied to them upon entry to this function.
fn unify_(st: &mut UnifySt, mut want: Ty, mut got: Ty) -> Result<()> {
  apply(&st.subst, &mut want);
  apply(&st.subst, &mut got);
  match (want, got) {
    (Ty::None, _) | (_, Ty::None) => Ok(()),
    (Ty::BoundVar(want), Ty::BoundVar(got)) => head_match(want == got),
    (Ty::MetaVar(mv), ty) | (ty, Ty::MetaVar(mv)) => unify_mv(st, mv, ty),
    (Ty::FixedVar(want), Ty::FixedVar(got)) => head_match(want == got),
    (Ty::Record(want_rows), Ty::Record(mut got_rows)) => {
      for (lab, want) in want_rows {
        match got_rows.remove(&lab) {
          None => return Err(UnifyError::HeadMismatch),
          Some(got) => unify_(st, want, got)?,
        }
      }
      if got_rows.is_empty() {
        Ok(())
      } else {
        Err(UnifyError::HeadMismatch)
      }
    }
    (Ty::Con(want_args, want_sym), Ty::Con(got_args, got_sym)) => {
      head_match(want_sym == got_sym)?;
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
    (Ty::BoundVar(_) | Ty::FixedVar(_) | Ty::Record(_) | Ty::Con(_, _) | Ty::Fn(_, _), _) => {
      Err(UnifyError::HeadMismatch)
    }
  }
}

fn unify_mv(st: &mut UnifySt, mv: MetaTyVar, ty: Ty) -> Result<()> {
  // return without doing anything if the meta vars are the same.
  if let Ty::MetaVar(mv2) = &ty {
    if mv == *mv2 {
      return Ok(());
    }
  }
  // forbid circularity.
  if occurs(&mv, &ty) {
    return Err(UnifyError::OccursCheck(mv, ty));
  }
  // solve mv to ty. however, mv may already have an entry.
  match st.subst.insert(mv, SubstEntry::Solved(ty.clone())) {
    // do nothing if no entry.
    None => {}
    // unreachable because we applied upon entry.
    Some(SubstEntry::Solved(ty)) => unreachable!("meta var already solved to {ty:?}"),
    Some(SubstEntry::Kind(kind)) => match kind {
      // TODO check ty is do more for equality checks
      TyVarKind::Equality => {}
      // mv was an overloaded ty var. ty must conform to that overload.
      TyVarKind::Overloaded(ov) => match ty {
        // don't emit more errors for None.
        Ty::None => {}
        // the simple case. check the sym is in the overload.
        Ty::Con(args, s) => {
          if ov
            .as_basics()
            .iter()
            .any(|&ov| st.overloads[ov].contains(&s))
          {
            assert!(args.is_empty())
          } else {
            return Err(UnifyError::OverloadMismatch(ov, mv));
          }
        }
        // we solved mv = mv2. now we give mv2 mv's old entry, to make it an overloaded ty var.
        // but mv2 itself may also have an entry.
        Ty::MetaVar(mv2) => {
          let ov = match st.subst.get(&mv2) {
            // it didn't have an entry.
            None => ov,
            // unreachable because of apply.
            Some(SubstEntry::Solved(ty)) => unreachable!("meta var already solved to {ty:?}"),
            Some(SubstEntry::Kind(kind)) => match kind {
              // all overload types are equality types.
              TyVarKind::Equality => ov,
              // it too was an overload. attempt to unify the two overloads.
              TyVarKind::Overloaded(ov2) => match ov.unify(*ov2) {
                Some(ov) => ov,
                None => return Err(UnifyError::OverloadMismatch(ov, mv)),
              },
              // no overloaded type is a record.
              TyVarKind::Record(_) => return Err(UnifyError::OverloadMismatch(ov, mv)),
            },
          };
          st.subst
            .insert(mv2, SubstEntry::Kind(TyVarKind::Overloaded(ov)));
        }
        // none of these are overloaded types.
        Ty::BoundVar(_) | Ty::FixedVar(_) | Ty::Record(_) | Ty::Fn(_, _) => {
          return Err(UnifyError::OverloadMismatch(ov, mv))
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
              None => return Err(UnifyError::HeadMismatch),
              Some(got) => unify_(st, want, got)?,
            }
          }
        }
        // ty was a meta var, so we solved mv = mv2. check if mv2 has its own entry before
        // setting mv2's entry to mv's old entry, which specifies the rows for this record ty
        // var.
        Ty::MetaVar(mv2) => {
          match st.subst.get(&mv2) {
            // there was no entry.
            None => {}
            // unreachable because of apply.
            Some(SubstEntry::Solved(ty)) => unreachable!("meta var already solved to {ty:?}"),
            Some(SubstEntry::Kind(kind)) => match kind {
              // TODO check if the rows so far are equality.
              TyVarKind::Equality => {}
              // no overloaded type is a record type.
              TyVarKind::Overloaded(_) => return Err(UnifyError::HeadMismatch),
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
          st.subst
            .insert(mv2, SubstEntry::Kind(TyVarKind::Record(want_rows)));
        }
        // none of these are record types.
        Ty::BoundVar(_) | Ty::FixedVar(_) | Ty::Con(_, _) | Ty::Fn(_, _) => {
          return Err(UnifyError::HeadMismatch)
        }
      },
    },
  }
  Ok(())
}

fn head_match(b: bool) -> Result<()> {
  if b {
    Ok(())
  } else {
    Err(UnifyError::HeadMismatch)
  }
}

fn occurs(mv: &MetaTyVar, ty: &Ty) -> bool {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::FixedVar(_) => false,
    Ty::MetaVar(mv2) => mv == mv2,
    Ty::Record(rows) => rows.values().any(|t| occurs(mv, t)),
    Ty::Con(args, _) => args.iter().any(|t| occurs(mv, t)),
    Ty::Fn(param, res) => occurs(mv, param) || occurs(mv, res),
  }
}
