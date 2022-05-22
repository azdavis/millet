use crate::cx::{Cx, Subst};
use crate::error::Error;
use crate::types::{MetaTyVar, Ty};
use crate::util::apply;

pub(crate) fn unify(cx: &mut Cx, want: Ty, got: Ty) {
  let e = match unify_(cx.subst(), want.clone(), got.clone()) {
    Ok(()) => return,
    Err(e) => e,
  };
  let e = match e {
    UnifyError::OccursCheck(mv, ty) => Error::Circularity(mv, ty),
    UnifyError::HeadMismatch => Error::MismatchedTypes(want, got),
    UnifyError::MissingField(lab) => Error::MissingField(lab, got),
    UnifyError::ExtraFields(labs) => Error::ExtraFields(labs, got),
  };
  cx.err(e);
}

#[derive(Debug)]
enum UnifyError {
  OccursCheck(MetaTyVar, Ty),
  HeadMismatch,
  MissingField(hir::Lab),
  ExtraFields(Vec<hir::Lab>),
}

/// `want` and `got` will have `subst` applied to them upon entry to this function
fn unify_(subst: &mut Subst, mut want: Ty, mut got: Ty) -> Result<(), UnifyError> {
  apply(subst, &mut want);
  apply(subst, &mut got);
  match (want, got) {
    (Ty::None, _) | (_, Ty::None) => Ok(()),
    (Ty::BoundVar(want), Ty::BoundVar(got)) => head_match(want == got),
    (Ty::MetaVar(mv), ty) | (ty, Ty::MetaVar(mv)) => {
      if let Ty::MetaVar(mv2) = ty {
        head_match(mv == mv2)
      } else if occurs(&mv, &ty) {
        Err(UnifyError::OccursCheck(mv, ty))
      } else {
        subst.insert(mv, ty);
        Ok(())
      }
    }
    (Ty::Record(want_rows), Ty::Record(mut got_rows)) => {
      for (lab, want) in want_rows {
        match got_rows.remove(&lab) {
          None => return Err(UnifyError::MissingField(lab)),
          Some(got) => unify_(subst, want, got)?,
        }
      }
      if got_rows.is_empty() {
        Ok(())
      } else {
        Err(UnifyError::ExtraFields(got_rows.into_keys().collect()))
      }
    }
    (Ty::Con(want_args, want_sym), Ty::Con(got_args, got_sym)) => {
      head_match(want_sym == got_sym)?;
      assert_eq!(want_args.len(), got_args.len());
      for (want, got) in want_args.into_iter().zip(got_args) {
        unify_(subst, want, got)?;
      }
      Ok(())
    }
    (Ty::Fn(want_param, want_res), Ty::Fn(got_param, got_res)) => {
      unify_(subst, *want_param, *got_param)?;
      unify_(subst, *want_res, *got_res)
    }
    (Ty::BoundVar(_) | Ty::Record(_) | Ty::Con(_, _) | Ty::Fn(_, _), _) => {
      Err(UnifyError::HeadMismatch)
    }
  }
}

fn head_match(b: bool) -> Result<(), UnifyError> {
  if b {
    Ok(())
  } else {
    Err(UnifyError::HeadMismatch)
  }
}

fn occurs(mv: &MetaTyVar, ty: &Ty) -> bool {
  match ty {
    Ty::None | Ty::BoundVar(_) => false,
    Ty::MetaVar(mv2) => mv == mv2,
    Ty::Record(rows) => rows.values().any(|t| occurs(mv, t)),
    Ty::Con(args, _) => args.iter().any(|t| occurs(mv, t)),
    Ty::Fn(param, res) => occurs(mv, param) || occurs(mv, res),
  }
}
