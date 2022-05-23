use crate::error::Error;
use crate::pat_match::{Con, Pat};
use crate::st::St;
use crate::ty;
use crate::types::{Ty, ValEnv};
use crate::unify::unify;
use crate::util::{apply, get_scon, record};

pub(crate) fn get(st: &mut St, ars: &hir::Arenas, ve: &mut ValEnv, pat: hir::PatIdx) -> (Pat, Ty) {
  match ars.pat[pat] {
    hir::Pat::None => (Pat::zero(Con::Any, pat), Ty::None),
    hir::Pat::Wild => (Pat::zero(Con::Any, pat), Ty::MetaVar(st.gen_meta_var())),
    hir::Pat::SCon(ref scon) => {
      let con = match *scon {
        hir::SCon::Int(i) => Con::Int(i),
        hir::SCon::Real(_) => {
          st.err(Error::RealPat);
          Con::Any
        }
        hir::SCon::Word(w) => Con::Word(w),
        hir::SCon::Char(c) => Con::Char(c),
        hir::SCon::String(ref s) => Con::String(s.clone()),
      };
      (Pat::zero(con, pat), get_scon(scon))
    }
    hir::Pat::Con(_, _) => todo!(),
    hir::Pat::Record {
      ref rows,
      allows_other,
    } => {
      if allows_other {
        todo!()
      }
      let mut labs = Vec::<hir::Lab>::with_capacity(rows.len());
      let mut pats = Vec::<Pat>::with_capacity(rows.len());
      let ty = record(st, rows, |st, lab, pat| {
        let (pm_pat, ty) = get(st, ars, ve, pat);
        labs.push(lab.clone());
        pats.push(pm_pat);
        ty
      });
      (Pat::con(Con::Record(labs), pats, pat), ty)
    }
    hir::Pat::Typed(pat, want) => {
      let (pm_pat, got) = get(st, ars, ve, pat);
      let mut want = ty::get(st, ars, want);
      unify(st, want.clone(), got);
      apply(st.subst(), &mut want);
      (pm_pat, want)
    }
    hir::Pat::As(_, _) => todo!(),
  }
}
