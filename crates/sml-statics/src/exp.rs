//! Checking expressions.

use crate::error::{AppendArg, ErrorKind};
use crate::get_env::{get_env_raw, get_val_info};
use crate::info::TyEntry;
use crate::util::record;
use crate::{config::Cfg, pat_match::Pat};
use crate::{dec, pat, st::St, ty, unify::unify};
use fast_hash::FxHashSet;
use sml_statics_types::env::{Cx, Env};
use sml_statics_types::sym::{Sym, SymsMarker};
use sml_statics_types::ty::{Generalizable, Ty, TyData, TyScheme, Tys};
use sml_statics_types::util::{get_scon, instantiate};
use sml_statics_types::{def, info::ValEnv, item::Item, mode::Mode};

pub(crate) fn get_and_check_ty_escape(
  st: &mut St<'_>,
  cfg: Cfg,
  cx: &Cx,
  marker: SymsMarker,
  ars: &sml_hir::Arenas,
  exp: sml_hir::ExpIdx,
) -> Ty {
  let ret = get(st, cfg, cx, ars, exp);
  if let (Some(exp), Some(ty)) = (exp, ty_escape(&st.syms_tys.tys, cx, marker, ret)) {
    st.err(exp, ErrorKind::TyEscape(ty));
  }
  ret
}

fn get(st: &mut St<'_>, cfg: Cfg, cx: &Cx, ars: &sml_hir::Arenas, exp: sml_hir::ExpIdx) -> Ty {
  let exp = match exp {
    Some(x) => x,
    None => return Ty::NONE,
  };
  // NOTE: do not early return, since we add to the Info at the bottom.
  let mut ty_scheme = None::<TyScheme>;
  let mut defs = FxHashSet::<def::Def>::default();
  let ret = match &ars.exp[exp] {
    sml_hir::Exp::Hole => {
      let mv = st.syms_tys.tys.meta_var(Generalizable::Always);
      st.err(exp, ErrorKind::ExpHole(mv));
      mv
    }
    // @def(1)
    sml_hir::Exp::SCon(scon) => get_scon(&mut st.syms_tys.tys, Generalizable::Always, scon),
    // @def(2)
    sml_hir::Exp::Path(path) => {
      let val_info = get_val_info(&cx.env, path);
      for e in val_info.disallow {
        st.err(exp, e.into());
      }
      match val_info.val {
        Err(e) => {
          st.err(exp, e.into());
          Ty::NONE
        }
        Ok(val_info) => {
          if let Some(d) = &val_info.disallow {
            st.err(exp, ErrorKind::Disallowed(Item::Val, d.clone(), path.last().clone()));
          }
          if let Mode::Dynamics = st.info.mode {
            assert!(st.exp_id_statuses.insert(exp, val_info.id_status).is_none());
          }
          ty_scheme = Some(val_info.ty_scheme.clone());
          defs.reserve(val_info.defs.len());
          for &def in &val_info.defs {
            defs.insert(def);
            if let def::Def::Path(_, idx) = def {
              st.mark_used(idx);
            }
          }
          instantiate(&mut st.syms_tys.tys, Generalizable::Always, &val_info.ty_scheme)
        }
      }
    }
    // @def(3)
    sml_hir::Exp::Record(rows) => {
      let rows = record(st, exp.into(), rows, |st, _, exp| get(st, cfg, cx, ars, exp));
      st.syms_tys.tys.record(rows)
    }
    // @def(4)
    sml_hir::Exp::Let(dec, inner) => {
      let mut let_env = Env::default();
      dec::get(st, cfg, cx, ars, &mut let_env, dec);
      let mut cx = cx.clone();
      cx.env.append(&mut let_env);
      get(st, cfg, &cx, ars, *inner)
    }
    // @def(8)
    sml_hir::Exp::App(func, argument) => {
      if let Some(ek) = lint_app(cx, ars, *func, *argument) {
        st.err(exp, ek);
      }
      let func_ty = get(st, cfg, cx, ars, *func);
      let arg_ty = get(st, cfg, cx, ars, *argument);
      // we could use the `_` case always, but it's slightly nicer if we know the function is
      // already a function type to just unify the parameter with the argument.
      match st.syms_tys.tys.data(func_ty) {
        TyData::Fn(data) => {
          unify(st, argument.unwrap_or(exp).into(), data.param, arg_ty);
          data.res
        }
        _ => {
          let ret = st.syms_tys.tys.meta_var(Generalizable::Always);
          let want = st.syms_tys.tys.fun(arg_ty, ret);
          unify(st, func.unwrap_or(exp).into(), want, func_ty);
          ret
        }
      }
    }
    // @def(10)
    sml_hir::Exp::Handle(inner, matcher) => {
      if !maybe_effectful(ars, *inner) {
        st.err(exp, ErrorKind::UnreachableHandle);
      }
      let exp_ty = get(st, cfg, cx, ars, *inner);
      let (pats, param, res) = get_matcher(st, exp.into(), cfg, cx, ars, matcher);
      let idx = inner.unwrap_or(exp);
      unify(st, idx.into(), Ty::EXN, param);
      unify(st, idx.into(), exp_ty, res);
      st.insert_handle(idx.into(), pats, param);
      exp_ty
    }
    // @def(11)
    sml_hir::Exp::Raise(inner) => {
      let got = get(st, cfg, cx, ars, *inner);
      unify(st, inner.unwrap_or(exp).into(), Ty::EXN, got);
      st.syms_tys.tys.meta_var(Generalizable::Always)
    }
    // @def(12)
    sml_hir::Exp::Fn(matcher, flavor) => {
      let (pats, param, res) = get_matcher(st, exp.into(), cfg, cx, ars, matcher);
      if let TyData::Con(data) = st.syms_tys.tys.data(param) {
        if data.sym == Sym::BOOL {
          assert!(data.args.is_empty(), "bool should have no ty args");
          if matches!(flavor, sml_hir::FnFlavor::Case) {
            st.err(exp, ErrorKind::BoolCase);
          }
        }
      }
      st.insert_case(exp.into(), pats, param);
      st.syms_tys.tys.fun(param, res)
    }
    // @def(9)
    sml_hir::Exp::Typed(inner, want, _) => {
      let got = get(st, cfg, cx, ars, *inner);
      let want = ty::get(st, cx, ars, ty::Mode::Regular, *want);
      unify(st, exp.into(), want, got);
      want
    }
  };
  st.info.entries.tys.exp.insert(exp, TyEntry::new(ret, ty_scheme));
  if !defs.is_empty() {
    st.info.entries.defs.exp.insert(exp, defs);
  }
  ret
}

fn lint_app(
  cx: &Cx,
  ars: &sml_hir::Arenas,
  func: sml_hir::ExpIdx,
  argument: sml_hir::ExpIdx,
) -> Option<ErrorKind> {
  match &ars.exp[func?] {
    sml_hir::Exp::Path(path) => {
      let mut iter =
        get_env_raw(&cx.env, path.prefix()).ok()?.val_env.get(path.last())?.defs.iter();
      match iter.next()? {
        def::Def::Primitive(_) => {
          assert!(iter.next().is_none(), "primitives should have exactly one def");
          assert!(path.prefix().is_empty(), "primitives are at the top level");
          match path.last().as_str() {
            "=" | "<>" => lint_eq(cx, ars, argument),
            "use" => {
              let file_name = argument.and_then(|arg| match &ars.exp[arg] {
                sml_hir::Exp::SCon(sml_hir::SCon::String(s)) => Some(s.clone()),
                _ => None,
              });
              Some(ErrorKind::Use(file_name))
            }
            _ => None,
          }
        }
        def::Def::Path(def::Path::BuiltinLib("std_basis/list.sml"), _) => {
          if path.last().as_str() == "@" {
            lint_append(ars, argument)
          } else {
            None
          }
        }
        def::Def::Path(_, _) => None,
      }
    }
    sml_hir::Exp::Fn(_, sml_hir::FnFlavor::Fn) => Some(ErrorKind::AppFn),
    _ => None,
  }
}

type SomeExpIdx = sml_hir::la_arena::Idx<sml_hir::Exp>;

fn get_pair(ars: &sml_hir::Arenas, idx: SomeExpIdx) -> Option<[SomeExpIdx; 2]> {
  match &ars.exp[idx] {
    sml_hir::Exp::Record(rows) => match rows.as_slice() {
      &[(sml_hir::Lab::Num(1), a), (sml_hir::Lab::Num(2), b)] => Some([a?, b?]),
      _ => None,
    },
    _ => None,
  }
}

/// not just for `=` but also `<>`.
fn lint_eq(cx: &Cx, ars: &sml_hir::Arenas, argument: sml_hir::ExpIdx) -> Option<ErrorKind> {
  get_pair(ars, argument?)?.into_iter().find_map(|argument| {
    let path = match &ars.exp[argument] {
      sml_hir::Exp::Path(p) => p,
      _ => return None,
    };
    let vi = get_env_raw(&cx.env, path.prefix()).ok()?.val_env.get(path.last())?;
    match vi.defs.iter().next()? {
      def::Def::Path(def::Path::BuiltinLib(_), _) | def::Def::Primitive(_) => {}
      def::Def::Path(def::Path::Regular(_), _) => return None,
    }
    match path.last().as_str() {
      "NONE" | "nil" | "true" | "false" => Some(ErrorKind::InvalidEq(path.last().clone())),
      _ => None,
    }
  })
}

fn lint_append(ars: &sml_hir::Arenas, argument: sml_hir::ExpIdx) -> Option<ErrorKind> {
  let [lhs, rhs] = get_pair(ars, argument?)?;
  if let sml_hir::Exp::Path(p) = &ars.exp[rhs] {
    if p.last().as_str() == "nil" {
      return Some(ErrorKind::InvalidAppend(AppendArg::Empty));
    }
  }
  let (func, argument) = match ars.exp[lhs] {
    sml_hir::Exp::App(a, b) => (a?, b?),
    sml_hir::Exp::Path(ref p) => {
      return (p.last().as_str() == "nil").then_some(ErrorKind::InvalidAppend(AppendArg::Empty))
    }
    _ => return None,
  };
  match &ars.exp[func] {
    sml_hir::Exp::Path(p) => {
      if p.last().as_str() != "::" {
        return None;
      }
    }
    _ => return None,
  }
  let [_, rhs] = get_pair(ars, argument)?;
  if let sml_hir::Exp::Path(p) = &ars.exp[rhs] {
    if p.last().as_str() == "nil" {
      return Some(ErrorKind::InvalidAppend(AppendArg::Singleton));
    }
  }
  None
}

pub(crate) fn maybe_effectful(ars: &sml_hir::Arenas, exp: sml_hir::ExpIdx) -> bool {
  let exp = match exp {
    Some(x) => x,
    None => return true,
  };
  match &ars.exp[exp] {
    sml_hir::Exp::SCon(_) | sml_hir::Exp::Path(_) | sml_hir::Exp::Fn(_, _) => false,
    sml_hir::Exp::Record(rows) => rows.iter().any(|&(_, exp)| maybe_effectful(ars, exp)),
    sml_hir::Exp::Typed(exp, _, _) => maybe_effectful(ars, *exp),
    sml_hir::Exp::Let(dec, exp) => dec::maybe_effectful(ars, dec) || maybe_effectful(ars, *exp),
    sml_hir::Exp::Hole
    | sml_hir::Exp::App(_, _)
    | sml_hir::Exp::Handle(_, _)
    | sml_hir::Exp::Raise(_) => true,
  }
}

/// @def(13)
fn get_matcher(
  st: &mut St<'_>,
  idx: sml_hir::Idx,
  cfg: Cfg,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  matcher: &[sml_hir::Arm],
) -> (Vec<Pat>, Ty, Ty) {
  let param_ty = st.syms_tys.tys.meta_var(Generalizable::Always);
  let res_ty = st.syms_tys.tys.meta_var(Generalizable::Always);
  let mut pats = Vec::<Pat>::new();
  st.syms_tys.tys.inc_meta_var_rank();
  // @def(14)
  for arm in matcher {
    let mut ve = ValEnv::default();
    let cfg = pat::Cfg { cfg, gen: Generalizable::Sometimes, rec: false };
    let (pm_pat, pat_ty) = pat::get(st, cfg, ars, cx, &mut ve, arm.pat);
    let mut cx = cx.clone();
    cx.env.val_env.append(&mut ve);
    let exp_ty = get(st, cfg.cfg, &cx, ars, arm.exp);
    unify(st, arm.pat.map_or(idx, Into::into), param_ty, pat_ty);
    unify(st, arm.exp.map_or(idx, Into::into), res_ty, exp_ty);
    pats.push(pm_pat);
  }
  st.syms_tys.tys.dec_meta_var_rank();
  (pats, param_ty, res_ty)
}

fn ty_escape(tys: &Tys, cx: &Cx, m: SymsMarker, ty: Ty) -> Option<Ty> {
  match tys.data(ty) {
    TyData::None | TyData::BoundVar(_) | TyData::UnsolvedMetaVar(_) => None,
    TyData::FixedVar(fv) => (!cx.fixed.contains_key(&fv.ty_var)).then_some(ty),
    TyData::Record(rows) => rows.values().find_map(|&ty| ty_escape(tys, cx, m, ty)),
    TyData::Con(data) => data
      .sym
      .generated_after(m)
      .then_some(ty)
      .or_else(|| data.args.iter().find_map(|&ty| ty_escape(tys, cx, m, ty))),
    TyData::Fn(data) => {
      ty_escape(tys, cx, m, data.param).or_else(|| ty_escape(tys, cx, m, data.res))
    }
  }
}
