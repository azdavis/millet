use crate::error::{ErrorKind, Item};
use crate::pat_match::Pat;
use crate::st::St;
use crate::types::{Cx, Env, Sym, SymsMarker, Ty, ValEnv};
use crate::unify::unify;
use crate::util::{apply, get_env, get_scon, instantiate, record};
use crate::{dec, pat, ty};
use std::sync::Arc;

pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, exp: hir::ExpIdx) -> Ty {
  let exp = match exp {
    Some(x) => x,
    None => return Ty::None,
  };
  match &ars.exp[exp] {
    // sml_def(1)
    hir::Exp::SCon(scon) => get_scon(scon),
    // sml_def(2)
    hir::Exp::Path(path) => {
      let env = match get_env(&cx.env, path.structures()) {
        Ok(x) => x,
        Err(name) => {
          st.err(exp, ErrorKind::Undefined(Item::Struct, name.clone()));
          return Ty::None;
        }
      };
      match env.val_env.get(path.last()) {
        Some(val_info) => instantiate(st, &val_info.ty_scheme),
        None => {
          st.err(exp, ErrorKind::Undefined(Item::Val, path.last().clone()));
          Ty::None
        }
      }
    }
    // sml_def(3)
    hir::Exp::Record(rows) => record(st, rows, exp, |st, _, exp| get(st, cx, ars, exp)),
    // sml_def(4)
    hir::Exp::Let(dec, inner) => {
      let mut let_env = Env::default();
      let marker = st.syms.mark();
      dec::get(st, cx, ars, &mut let_env, *dec);
      let mut cx = cx.clone();
      Arc::make_mut(&mut cx.env).extend(let_env);
      let got = get(st, &cx, ars, *inner);
      if let Some(sym) = ty_name_escape(&marker, &got) {
        st.err(inner.unwrap_or(exp), ErrorKind::TyNameEscape(sym));
      }
      got
    }
    // sml_def(8)
    hir::Exp::App(func, arg) => {
      // TODO do func before arg. it's in this order right now to cause error emission order for
      // exp seq/case lowering to be slightly better
      let arg_ty = get(st, cx, ars, *arg);
      let func_ty = get(st, cx, ars, *func);
      // we could choose to not `match` on `func_ty` and just use the `MetaVar` case always and it
      // would still be correct. however, matching on `func_ty` lets us emit slightly better error
      // messages.
      match func_ty {
        Ty::None => Ty::None,
        Ty::BoundVar(_) => unreachable!(),
        Ty::MetaVar(_) => {
          let mut ret = Ty::MetaVar(st.gen_meta_var());
          let got = Ty::fun(arg_ty, ret.clone());
          unify(st, func_ty, got, exp);
          apply(st.subst(), &mut ret);
          ret
        }
        Ty::FixedVar(_) | Ty::Record(_) | Ty::Con(_, _) => {
          st.err(func.unwrap_or(exp), ErrorKind::AppLhsNotFn(func_ty));
          Ty::None
        }
        Ty::Fn(want_arg, mut want_res) => {
          unify(st, *want_arg, arg_ty, arg.unwrap_or(exp));
          apply(st.subst(), want_res.as_mut());
          *want_res
        }
      }
    }
    // sml_def(10)
    hir::Exp::Handle(inner, matcher) => {
      let mut exp_ty = get(st, cx, ars, *inner);
      let (pats, param, res) = get_matcher(st, cx, ars, matcher, exp.into());
      let idx = inner.unwrap_or(exp);
      unify(st, Ty::EXN, param.clone(), idx);
      unify(st, exp_ty.clone(), res, idx);
      apply(st.subst(), &mut exp_ty);
      pat::get_match(st, pats, param, None, idx);
      exp_ty
    }
    // sml_def(11)
    hir::Exp::Raise(inner) => {
      let got = get(st, cx, ars, *inner);
      unify(st, Ty::EXN, got, inner.unwrap_or(exp));
      Ty::MetaVar(st.gen_meta_var())
    }
    // sml_def(12)
    hir::Exp::Fn(matcher) => {
      let (pats, param, res) = get_matcher(st, cx, ars, matcher, exp.into());
      pat::get_match(
        st,
        pats,
        param.clone(),
        Some(ErrorKind::NonExhaustiveCase),
        exp,
      );
      Ty::fun(param, res)
    }
    // sml_def(9)
    hir::Exp::Typed(inner, want) => {
      let got = get(st, cx, ars, *inner);
      let mut want = ty::get(st, cx, ars, *want);
      unify(st, want.clone(), got, exp);
      apply(st.subst(), &mut want);
      want
    }
  }
}

/// sml_def(13)
fn get_matcher(
  st: &mut St,
  cx: &Cx,
  ars: &hir::Arenas,
  matcher: &[(hir::PatIdx, hir::ExpIdx)],
  idx: hir::Idx,
) -> (Vec<Pat>, Ty, Ty) {
  let mut param_ty = Ty::MetaVar(st.gen_meta_var());
  let mut res_ty = Ty::MetaVar(st.gen_meta_var());
  let mut pats = Vec::<Pat>::new();
  // sml_def(14)
  for &(pat, exp) in matcher {
    let mut ve = ValEnv::default();
    let (pm_pat, pat_ty) = pat::get(st, cx, ars, &mut ve, pat);
    let mut cx = cx.clone();
    Arc::make_mut(&mut cx.env).val_env.extend(ve);
    let exp_ty = get(st, &cx, ars, exp);
    let pi = pat.map_or(idx, Into::into);
    unify(st, param_ty.clone(), pat_ty, pi);
    let ei = exp.map_or(idx, Into::into);
    unify(st, res_ty.clone(), exp_ty, ei);
    apply(st.subst(), &mut param_ty);
    apply(st.subst(), &mut res_ty);
    pats.push(pm_pat);
  }
  (pats, param_ty, res_ty)
}

fn ty_name_escape(m: &SymsMarker, ty: &Ty) -> Option<Sym> {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) => None,
    Ty::Record(rows) => rows.values().find_map(|ty| ty_name_escape(m, ty)),
    Ty::Con(args, sym) => sym
      .generated_after(m)
      .then(|| *sym)
      .or_else(|| args.iter().find_map(|ty| ty_name_escape(m, ty))),
    Ty::Fn(param, res) => ty_name_escape(m, param).or_else(|| ty_name_escape(m, res)),
  }
}
