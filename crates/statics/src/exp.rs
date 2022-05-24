use crate::error::ErrorKind;
use crate::pat_match::Pat;
use crate::st::St;
use crate::types::{Cx, Env, Sym, SymsMarker, Ty, ValEnv};
use crate::unify::unify;
use crate::util::{apply, get_env, get_scon, instantiate, record};
use crate::{dec, pat, ty};

pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, exp: hir::ExpIdx) -> Ty {
  match &ars.exp[exp] {
    hir::Exp::None => Ty::None,
    hir::Exp::SCon(scon) => get_scon(scon),
    hir::Exp::Path(path) => {
      let env = match get_env(&cx.env, path) {
        Ok(x) => x,
        Err(_) => {
          st.err(ErrorKind::Undefined);
          return Ty::None;
        }
      };
      match env.val_env.get(path.last()) {
        Some(val_info) => instantiate(st, &val_info.ty_scheme),
        None => {
          st.err(ErrorKind::Undefined);
          Ty::None
        }
      }
    }
    hir::Exp::Record(rows) => record(st, rows, |st, _, exp| get(st, cx, ars, exp)),
    hir::Exp::Let(dec, exp) => {
      let mut env = Env::default();
      let marker = st.syms.mark();
      dec::get(st, cx, ars, &mut env, *dec);
      let mut cx = cx.clone();
      cx.env.extend(env);
      let got = get(st, &cx, ars, *exp);
      if ty_name_escape(&marker, &got) {
        st.err(ErrorKind::TyNameEscape);
      }
      got
    }
    hir::Exp::App(func, arg) => {
      let want = get(st, cx, ars, *func);
      let arg_ty = get(st, cx, ars, *arg);
      let mut res_ty = Ty::MetaVar(st.gen_meta_var());
      let got = Ty::Fn(arg_ty.into(), res_ty.clone().into());
      unify(st, want, got);
      apply(st.subst(), &mut res_ty);
      res_ty
    }
    hir::Exp::Handle(exp, matcher) => {
      let mut exp_ty = get(st, cx, ars, *exp);
      let (pats, param, res) = get_matcher(st, cx, ars, matcher);
      unify(st, Ty::zero(Sym::EXN), param.clone());
      unify(st, exp_ty.clone(), res);
      apply(st.subst(), &mut exp_ty);
      pat::get_match(st, pats, param, None);
      exp_ty
    }
    hir::Exp::Raise(exp) => {
      let got = get(st, cx, ars, *exp);
      unify(st, Ty::zero(Sym::EXN), got);
      Ty::MetaVar(st.gen_meta_var())
    }
    hir::Exp::Fn(matcher) => {
      let (pats, param, res) = get_matcher(st, cx, ars, matcher);
      pat::get_match(st, pats, param.clone(), Some(ErrorKind::NonExhaustiveMatch));
      Ty::Fn(param.into(), res.into())
    }
    hir::Exp::Typed(exp, want) => {
      let got = get(st, cx, ars, *exp);
      let mut want = ty::get(st, cx, ars, *want);
      unify(st, want.clone(), got);
      apply(st.subst(), &mut want);
      want
    }
  }
}

fn get_matcher(
  st: &mut St,
  cx: &Cx,
  ars: &hir::Arenas,
  matcher: &[(hir::PatIdx, hir::ExpIdx)],
) -> (Vec<Pat>, Ty, Ty) {
  let mut param_ty = Ty::MetaVar(st.gen_meta_var());
  let mut res_ty = Ty::MetaVar(st.gen_meta_var());
  let mut pats = Vec::<Pat>::new();
  for &(pat, exp) in matcher {
    let mut ve = ValEnv::default();
    let (pm_pat, pat_ty) = pat::get(st, cx, ars, &mut ve, pat);
    let mut cx = cx.clone();
    cx.env.val_env.extend(ve);
    let exp_ty = get(st, &cx, ars, exp);
    unify(st, param_ty.clone(), pat_ty);
    unify(st, res_ty.clone(), exp_ty);
    apply(st.subst(), &mut param_ty);
    apply(st.subst(), &mut res_ty);
    pats.push(pm_pat);
  }
  (pats, param_ty, res_ty)
}

fn ty_name_escape(m: &SymsMarker, ty: &Ty) -> bool {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) => false,
    Ty::Record(rows) => rows.values().any(|ty| ty_name_escape(m, ty)),
    Ty::Con(args, sym) => sym.generated_after(m) || args.iter().any(|ty| ty_name_escape(m, ty)),
    Ty::Fn(param, res) => ty_name_escape(m, param) || ty_name_escape(m, res),
  }
}
