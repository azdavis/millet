use crate::cx::Cx;
use crate::error::Error;
use crate::pat_match::{Lang, Pat};
use crate::types::{Sym, Ty, ValEnv};
use crate::unify::unify;
use crate::util::{apply, get_scon, record};
use crate::{pat, ty};

pub(crate) fn get(cx: &mut Cx, ars: &hir::Arenas, exp: hir::ExpIdx) -> Ty {
  match ars.exp[exp] {
    hir::Exp::None => Ty::None,
    hir::Exp::SCon(ref scon) => get_scon(scon),
    hir::Exp::Path(_) => todo!(),
    hir::Exp::Record(ref rows) => record(cx, rows, |cx, _, exp| get(cx, ars, exp)),
    hir::Exp::Let(_, _) => todo!(),
    hir::Exp::App(func, arg) => {
      let want = get(cx, ars, func);
      let arg_ty = get(cx, ars, arg);
      let mut res_ty = Ty::MetaVar(cx.gen_meta_var());
      let got = Ty::Fn(arg_ty.into(), res_ty.clone().into());
      unify(cx, want, got);
      apply(cx.subst(), &mut res_ty);
      res_ty
    }
    hir::Exp::Handle(_, _) => todo!(),
    hir::Exp::Raise(exp) => {
      let got = get(cx, ars, exp);
      unify(cx, Ty::zero(Sym::EXN), got);
      Ty::MetaVar(cx.gen_meta_var())
    }
    hir::Exp::Fn(ref matcher) => {
      let (pats, param, res) = get_matcher(cx, ars, matcher);
      ck_pat_match(cx, pats, param.clone(), Some(Error::NonExhaustiveMatch));
      Ty::Fn(param.into(), res.into())
    }
    hir::Exp::Typed(exp, want) => {
      let got = get(cx, ars, exp);
      let mut want = ty::get(cx, ars, want);
      unify(cx, want.clone(), got);
      apply(cx.subst(), &mut want);
      want
    }
  }
}

fn get_matcher(
  cx: &mut Cx,
  ars: &hir::Arenas,
  matcher: &[(hir::PatIdx, hir::ExpIdx)],
) -> (Vec<Pat>, Ty, Ty) {
  let mut param_ty = Ty::MetaVar(cx.gen_meta_var());
  let mut res_ty = Ty::MetaVar(cx.gen_meta_var());
  let mut pats = Vec::<Pat>::new();
  for &(pat, exp) in matcher {
    let mut ve = ValEnv::default();
    let (pm_pat, pat_ty) = pat::get(cx, ars, &mut ve, pat);
    // TODO extend env for exp_ty with the ve?
    let exp_ty = get(cx, ars, exp);
    unify(cx, param_ty.clone(), pat_ty);
    unify(cx, res_ty.clone(), exp_ty);
    apply(cx.subst(), &mut param_ty);
    apply(cx.subst(), &mut res_ty);
    pats.push(pm_pat);
  }
  (pats, param_ty, res_ty)
}

fn ck_pat_match(cx: &mut Cx, pats: Vec<Pat>, ty: Ty, f: Option<fn(Vec<Pat>) -> Error>) {
  // TODO this could probably be done with borrows instead.
  let lang = Lang {
    syms: cx.take_syms(),
  };
  let ck = pattern_match::check(&lang, pats, ty);
  cx.set_syms(lang.syms);
  let mut unreachable: Vec<_> = ck.unreachable.into_iter().collect();
  unreachable.sort_unstable_by_key(|x| x.into_raw());
  for un in unreachable {
    cx.err(Error::UnreachablePattern(un));
  }
  if !ck.missing.is_empty() {
    if let Some(f) = f {
      cx.err(f(ck.missing));
    }
  }
}
