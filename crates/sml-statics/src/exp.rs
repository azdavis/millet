//! Checking expressions.

use crate::env::{Cx, Env, EnvLike as _};
use crate::error::{AppendArg, ErrorKind, Item};
use crate::get_env::get_val_info;
use crate::types::{Generalizable, Sym, SymsMarker, Ty, TyScheme, ValEnv};
use crate::util::{apply, get_scon, instantiate, record};
use crate::{config::Cfg, info::TyEntry, pat_match::Pat};
use crate::{dec, def, pat, st::St, ty, unify::unify};
use fast_hash::FxHashSet;

pub(crate) fn get_and_check_ty_escape(
  st: &mut St,
  cfg: Cfg,
  cx: &Cx,
  marker: SymsMarker,
  ars: &sml_hir::Arenas,
  exp: sml_hir::ExpIdx,
) -> Ty {
  let ret = get(st, cfg, cx, ars, exp);
  if let (Some(exp), Some(ty)) = (exp, ty_escape(cx, marker, &ret)) {
    st.err(exp, ErrorKind::TyEscape(ty));
  }
  ret
}

fn get(st: &mut St, cfg: Cfg, cx: &Cx, ars: &sml_hir::Arenas, exp: sml_hir::ExpIdx) -> Ty {
  let exp = match exp {
    Some(x) => x,
    None => return Ty::None,
  };
  // NOTE: do not early return, since we add to the Info at the bottom.
  let mut ty_scheme = None::<TyScheme>;
  let mut defs = FxHashSet::<def::Def>::default();
  let ret = match &ars.exp[exp] {
    sml_hir::Exp::Hole => {
      let mv = st.meta_gen.gen(Generalizable::Always);
      st.insert_hole(exp.into(), mv);
      Ty::MetaVar(mv)
    }
    // @def(1)
    sml_hir::Exp::SCon(scon) => get_scon(st, Generalizable::Always, scon),
    // @def(2)
    sml_hir::Exp::Path(path) => match get_val_info(&cx.env, path) {
      Ok(Some(val_info)) => {
        ty_scheme = Some(val_info.ty_scheme.clone());
        defs.reserve(val_info.defs.len());
        for &def in &val_info.defs {
          defs.insert(def);
          if let def::Def::Path(_, idx) = def {
            st.mark_used(idx);
          }
        }
        instantiate(st, Generalizable::Always, val_info.ty_scheme.clone())
      }
      Ok(None) => {
        st.err(exp, ErrorKind::Undefined(Item::Val, path.last().clone()));
        Ty::None
      }
      Err(e) => {
        st.err(exp, e);
        Ty::None
      }
    },
    // @def(3)
    sml_hir::Exp::Record(rows) => {
      let rows = record(st, exp.into(), rows, |st, _, exp| get(st, cfg, cx, ars, exp));
      Ty::Record(rows)
    }
    // @def(4)
    sml_hir::Exp::Let(dec, inner) => {
      let mut let_env = Env::default();
      dec::get(st, cfg, cx, ars, &mut let_env, *dec);
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
      match func_ty {
        Ty::Fn(param_ty, mut ret) => {
          unify(st, argument.unwrap_or(exp).into(), *param_ty, arg_ty);
          apply(&st.subst, ret.as_mut());
          *ret
        }
        _ => {
          let mut ret = Ty::MetaVar(st.meta_gen.gen(Generalizable::Always));
          let want = Ty::fun(arg_ty, ret.clone());
          unify(st, func.unwrap_or(exp).into(), want, func_ty);
          apply(&st.subst, &mut ret);
          ret
        }
      }
    }
    // @def(10)
    sml_hir::Exp::Handle(inner, matcher) => {
      if !maybe_effectful(ars, *inner) {
        st.err(exp, ErrorKind::UnreachableHandle);
      }
      let mut exp_ty = get(st, cfg, cx, ars, *inner);
      let (pats, param, res) = get_matcher(st, exp.into(), cfg, cx, ars, matcher);
      let idx = inner.unwrap_or(exp);
      unify(st, idx.into(), Ty::EXN, param.clone());
      unify(st, idx.into(), exp_ty.clone(), res);
      apply(&st.subst, &mut exp_ty);
      st.insert_handle(idx.into(), pats, param);
      exp_ty
    }
    // @def(11)
    sml_hir::Exp::Raise(inner) => {
      let got = get(st, cfg, cx, ars, *inner);
      unify(st, inner.unwrap_or(exp).into(), Ty::EXN, got);
      Ty::MetaVar(st.meta_gen.gen(Generalizable::Always))
    }
    // @def(12)
    sml_hir::Exp::Fn(matcher, flavor) => {
      let (pats, param, res) = get_matcher(st, exp.into(), cfg, cx, ars, matcher);
      if let Ty::Con(arguments, sym) = &param {
        if *sym == Sym::BOOL {
          assert!(arguments.is_empty(), "bool should have no ty args");
          if matches!(flavor, sml_hir::FnFlavor::Case) {
            st.err(exp, ErrorKind::BoolCase);
          }
        }
      }
      st.insert_case(exp.into(), pats, param.clone());
      Ty::fun(param, res)
    }
    // @def(9)
    sml_hir::Exp::Typed(inner, want) => {
      let got = get(st, cfg, cx, ars, *inner);
      let mut want = ty::get(st, cx, ars, ty::Mode::Regular, *want);
      unify(st, exp.into(), want.clone(), got);
      apply(&st.subst, &mut want);
      want
    }
  };
  let ty_entry = TyEntry::new(ret.clone(), ty_scheme);
  st.info.insert(exp.into(), Some(ty_entry), defs);
  ret
}

fn lint_app(
  cx: &Cx,
  ars: &sml_hir::Arenas,
  func: sml_hir::ExpIdx,
  argument: sml_hir::ExpIdx,
) -> Option<ErrorKind> {
  match &ars.exp[func?] {
    // just use iter().next() because if it's something we care about (a primitive path or builtin
    // lib path), the exp should only have one def anyway.
    sml_hir::Exp::Path(path) => match get_val_info(&cx.env, path).ok()??.defs.iter().next()? {
      def::Def::Primitive(_) => {
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
    },
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
    let vi = get_val_info(&cx.env, path).ok()??;
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
    sml_hir::Exp::Typed(exp, _) => maybe_effectful(ars, *exp),
    sml_hir::Exp::Let(dec, exp) => dec::maybe_effectful(ars, *dec) || maybe_effectful(ars, *exp),
    sml_hir::Exp::Hole
    | sml_hir::Exp::App(_, _)
    | sml_hir::Exp::Handle(_, _)
    | sml_hir::Exp::Raise(_) => true,
  }
}

/// @def(13)
fn get_matcher(
  st: &mut St,
  idx: sml_hir::Idx,
  cfg: Cfg,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  matcher: &[(sml_hir::PatIdx, sml_hir::ExpIdx)],
) -> (Vec<Pat>, Ty, Ty) {
  let mut param_ty = Ty::MetaVar(st.meta_gen.gen(Generalizable::Always));
  let mut res_ty = Ty::MetaVar(st.meta_gen.gen(Generalizable::Always));
  let mut pats = Vec::<Pat>::new();
  st.meta_gen.inc_rank();
  // @def(14)
  for &(pat, exp) in matcher {
    let mut ve = ValEnv::default();
    let cfg = pat::Cfg { cfg, gen: Generalizable::Sometimes, rec: false };
    let (pm_pat, pat_ty) = pat::get(st, cfg, ars, cx, &mut ve, pat);
    let mut cx = cx.clone();
    cx.env.push(Env { val_env: ve, ..Default::default() });
    let exp_ty = get(st, cfg.cfg, &cx, ars, exp);
    unify(st, pat.map_or(idx, Into::into), param_ty.clone(), pat_ty);
    unify(st, exp.map_or(idx, Into::into), res_ty.clone(), exp_ty);
    apply(&st.subst, &mut param_ty);
    apply(&st.subst, &mut res_ty);
    pats.push(pm_pat);
  }
  st.meta_gen.dec_rank();
  (pats, param_ty, res_ty)
}

fn ty_escape(cx: &Cx, m: SymsMarker, ty: &Ty) -> Option<Ty> {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) => None,
    Ty::FixedVar(fv) => (!cx.fixed.contains_key(fv.ty_var())).then(|| ty.clone()),
    Ty::Record(rows) => rows.values().find_map(|ty| ty_escape(cx, m, ty)),
    Ty::Con(args, sym) => sym
      .generated_after(m)
      .then(|| ty.clone())
      .or_else(|| args.iter().find_map(|ty| ty_escape(cx, m, ty))),
    Ty::Fn(param, res) => ty_escape(cx, m, param).or_else(|| ty_escape(cx, m, res)),
  }
}
