use crate::error::Error;
use crate::pat_match::{Lang, Pat};
use crate::st::St;
use crate::types::{Sym, Ty, ValEnv};
use crate::unify::unify;
use crate::util::{apply, get_scon, record};
use crate::{pat, ty};

pub(crate) fn get(st: &mut St, ars: &hir::Arenas, exp: hir::ExpIdx) -> Ty {
  match ars.exp[exp] {
    hir::Exp::None => Ty::None,
    hir::Exp::SCon(ref scon) => get_scon(scon),
    hir::Exp::Path(_) => todo!(),
    hir::Exp::Record(ref rows) => record(st, rows, |st, _, exp| get(st, ars, exp)),
    hir::Exp::Let(_, _) => todo!(),
    hir::Exp::App(func, arg) => {
      let want = get(st, ars, func);
      let arg_ty = get(st, ars, arg);
      let mut res_ty = Ty::MetaVar(st.gen_meta_var());
      let got = Ty::Fn(arg_ty.into(), res_ty.clone().into());
      unify(st, want, got);
      apply(st.subst(), &mut res_ty);
      res_ty
    }
    hir::Exp::Handle(exp, ref matcher) => {
      let mut exp_ty = get(st, ars, exp);
      let (pats, param, res) = get_matcher(st, ars, matcher);
      unify(st, Ty::zero(Sym::EXN), param.clone());
      unify(st, exp_ty.clone(), res);
      apply(st.subst(), &mut exp_ty);
      ck_pat_match(st, pats, param, None);
      exp_ty
    }
    hir::Exp::Raise(exp) => {
      let got = get(st, ars, exp);
      unify(st, Ty::zero(Sym::EXN), got);
      Ty::MetaVar(st.gen_meta_var())
    }
    hir::Exp::Fn(ref matcher) => {
      let (pats, param, res) = get_matcher(st, ars, matcher);
      ck_pat_match(st, pats, param.clone(), Some(Error::NonExhaustiveMatch));
      Ty::Fn(param.into(), res.into())
    }
    hir::Exp::Typed(exp, want) => {
      let got = get(st, ars, exp);
      let mut want = ty::get(st, ars, want);
      unify(st, want.clone(), got);
      apply(st.subst(), &mut want);
      want
    }
  }
}

fn get_matcher(
  st: &mut St,
  ars: &hir::Arenas,
  matcher: &[(hir::PatIdx, hir::ExpIdx)],
) -> (Vec<Pat>, Ty, Ty) {
  let mut param_ty = Ty::MetaVar(st.gen_meta_var());
  let mut res_ty = Ty::MetaVar(st.gen_meta_var());
  let mut pats = Vec::<Pat>::new();
  for &(pat, exp) in matcher {
    let mut ve = ValEnv::default();
    let (pm_pat, pat_ty) = pat::get(st, ars, &mut ve, pat);
    // TODO extend env for exp_ty with the ve?
    let exp_ty = get(st, ars, exp);
    unify(st, param_ty.clone(), pat_ty);
    unify(st, res_ty.clone(), exp_ty);
    apply(st.subst(), &mut param_ty);
    apply(st.subst(), &mut res_ty);
    pats.push(pm_pat);
  }
  (pats, param_ty, res_ty)
}

fn ck_pat_match(st: &mut St, pats: Vec<Pat>, ty: Ty, f: Option<fn(Vec<Pat>) -> Error>) {
  // TODO this could probably be done with borrows instead.
  let lang = Lang {
    syms: st.take_syms(),
  };
  let ck = pattern_match::check(&lang, pats, ty);
  st.set_syms(lang.syms);
  let mut unreachable: Vec<_> = ck.unreachable.into_iter().collect();
  unreachable.sort_unstable_by_key(|x| x.into_raw());
  for un in unreachable {
    st.err(Error::UnreachablePattern(un));
  }
  if !ck.missing.is_empty() {
    if let Some(f) = f {
      st.err(f(ck.missing));
    }
  }
}
