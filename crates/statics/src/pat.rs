use crate::cx::Cx;
use crate::ty;
use crate::types::{Ty, ValEnv};
use crate::unify::unify;
use crate::util::{apply, get_scon, record};

pub(crate) fn get(cx: &mut Cx, ars: &hir::Arenas, ve: &mut ValEnv, pat: hir::PatIdx) -> Ty {
  match ars.pat[pat] {
    hir::Pat::None => Ty::None,
    hir::Pat::Wild => Ty::MetaVar(cx.gen_meta_var()),
    hir::Pat::SCon(ref scon) => get_scon(scon),
    hir::Pat::Con(_, _) => todo!(),
    hir::Pat::Record {
      ref rows,
      allows_other,
    } => {
      if allows_other {
        todo!()
      }
      record(cx, rows, |cx, pat| get(cx, ars, ve, pat))
    }
    hir::Pat::Typed(pat, want) => {
      let got = get(cx, ars, ve, pat);
      let mut want = ty::get(cx, ars, want);
      unify(cx, want.clone(), got);
      apply(cx.subst(), &mut want);
      want
    }
    hir::Pat::As(_, _) => todo!(),
  }
}
