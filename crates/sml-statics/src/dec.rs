//! Checking declarations.

use crate::error::ErrorKind;
use crate::get_env::{get_env, get_env_raw, get_ty_info, get_val_info};
use crate::util::{check_name, ins_check_name, ins_no_dupe};
use crate::{config::Cfg, exp, pat, pat_match::Pat, st::St, ty, unify::unify};
use fast_hash::{FxHashMap, FxHashSet};
use sml_statics_types::env::{Cx, Env};
use sml_statics_types::info::{IdStatus, TyEnv, TyInfo, ValEnv, ValInfo};
use sml_statics_types::sym::{Equality, StartedSym, SymValEnv};
use sml_statics_types::ty::{Generalizable, Ty, TyData, TyScheme, TyVarSrc};
use sml_statics_types::{equality, generalize, item::Item};

pub(crate) fn get(
  st: &mut St<'_>,
  cfg: Cfg,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  env: &mut Env,
  decs: &[sml_hir::DecIdx],
) {
  match decs[..] {
    [] => return,
    [x] => {
      get_one(st, cfg, cx, ars, env, x);
      return;
    }
    _ => {}
  }
  // @def(23), @def(24)
  let mut cx = cx.clone();
  let mut one_env = Env::default();
  for &dec in decs {
    get_one(st, cfg, &cx, ars, &mut one_env, dec);
    cx.env.append(&mut one_env.clone());
    env.append(&mut one_env);
  }
}

fn get_one(
  st: &mut St<'_>,
  cfg: Cfg,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  env: &mut Env,
  dec: sml_hir::DecIdx,
) {
  match &ars.dec[dec] {
    // @def(15)
    sml_hir::Dec::Val(ty_vars, val_binds, _) => {
      if !maybe_effectful_val(ars, val_binds) {
        st.err(dec, ErrorKind::DecWithoutEffect);
      }
      let mut cx = cx.clone();
      let fixed = add_fixed_ty_vars(st, dec.into(), &mut cx, TyVarSrc::Val, ty_vars);
      // we actually resort to indexing logic because this is a little weird:
      // - we represent the recursive nature of ValBinds (and all other things that recurse with
      //   `and`) as a sequence of non-recursive items.
      // - if a ValBind is `rec`, it's not just this one, it's all the rest of them.
      // - we need to go over the recursive ValBinds twice.
      let mut idx = 0usize;
      let mut ve = ValEnv::default();
      let mut src_exp = FxHashMap::<str_util::Name, sml_hir::ExpIdx>::default();
      let exp_cfg = Cfg { mark_defined: true };
      let mut pat_cfg = pat::Cfg { cfg, gen: Generalizable::Sometimes, rec: false };
      let marker = st.syms_tys.syms.mark();
      while let Some(val_bind) = val_binds.get(idx) {
        if val_bind.rec {
          // this and all other remaining ones are recursive.
          break;
        }
        idx += 1;
        // @def(25)
        let (pm_pat, want) =
          get_pat_and_src_exp(st, pat_cfg, &cx, ars, &mut ve, val_bind, &mut src_exp);
        let got = exp::get_and_check_ty_escape(st, exp_cfg, &cx, marker, ars, val_bind.exp);
        unify(st, dec.into(), want, got);
        st.insert_bind(val_bind.pat.map_or(sml_hir::Idx::from(dec), Into::into), pm_pat, want);
      }
      // deal with the recursive ones. first do all the patterns so we can update the ValEnv. we
      // also need a separate recursive-only ValEnv.
      let mut rec_ve = ValEnv::default();
      pat_cfg.rec = true;
      let got_pats: Vec<_> = val_binds[idx..]
        .iter()
        .map(|val_bind| {
          get_pat_and_src_exp(st, pat_cfg, &cx, ars, &mut rec_ve, val_bind, &mut src_exp)
        })
        .collect();
      // merge the recursive and non-recursive ValEnvs, making sure they don't clash.
      for (name, val_info) in rec_ve.iter() {
        if let Some(e) = ins_no_dupe(&mut ve, name.clone(), val_info.clone(), Item::Val) {
          st.err(dec, e);
        }
      }
      // extend the cx with only the recursive ValEnv.
      cx.env.val_env.append(&mut rec_ve);
      // check each recursive expression with all of the recursive bindings in scope.
      for (val_bind, (pm_pat, want)) in val_binds[idx..].iter().zip(got_pats) {
        // @def(26)
        if !maybe_fn(&ars.exp, val_bind.exp) {
          st.err(dec, ErrorKind::ValRecExpNotFn);
        }
        let got = exp::get_and_check_ty_escape(st, exp_cfg, &cx, marker, ars, val_bind.exp);
        // if it's recursive, it might be referencing itself in the `fn`, so don't lint this.
        if let Some(e) = val_bind.exp {
          st.mark_eta_reduce_unable(e);
        }
        unify(st, dec.into(), want, got);
        st.insert_bind(dec.into(), pm_pat, want);
      }
      let mut generalized = FxHashSet::<sml_hir::ExpIdx>::default();
      // generalize the entire merged ValEnv.
      for (name, val_info) in ve.iter_mut() {
        let &exp = src_exp.get(name).expect("should have an exp for every bound name");
        if !generalized.insert(exp) {
          continue;
        }
        match generalize::get(&mut st.syms_tys.tys, fixed.clone(), val_info.ty_scheme.ty) {
          Ok(ts) => val_info.ty_scheme = ts,
          Err(ur) => st.err(ur.idx, ErrorKind::UnresolvedRecordTy(ur.rows)),
        }
        let Some(e) = exp else { continue };
        if val_info.ty_scheme.bound_vars.is_empty() {
          continue;
        }
        st.mark_eta_reduce_unable(e);
        if expansive(&cx, ars, exp) {
          st.err(e, ErrorKind::BindPolymorphicExpansiveExp);
        }
      }
      // extend the overall env with that.
      env.val_env.append(&mut ve);
    }
    // @def(16)
    sml_hir::Dec::Ty(ty_binds) => {
      let mut cx = cx.clone();
      let mut ty_env = TyEnv::default();
      // @def(27)
      get_ty_binds(st, dec.into(), &mut cx, ars, &mut ty_env, ty_binds);
      env.ty_env.append(&mut ty_env);
    }
    // @def(17)
    sml_hir::Dec::Datatype(dat_binds, with_types) => {
      let (mut ty_env, mut big_val_env) =
        get_dat_binds(st, dec.into(), cx.clone(), ars, dat_binds, with_types);
      env.ty_env.append(&mut ty_env);
      env.val_env.append(&mut big_val_env);
    }
    // @def(18)
    sml_hir::Dec::DatatypeCopy(name, path) => {
      let ty_info = get_ty_info(&cx.env, path);
      for e in ty_info.disallow {
        st.err(dec, e.into());
      }
      match ty_info.val {
        Ok(ty_info) => {
          env.ty_env.insert(name.clone(), ty_info.clone());
          env.val_env.append(&mut ty_info.val_env.clone());
        }
        Err(e) => st.err(dec, e.into()),
      }
    }
    // @def(19)
    sml_hir::Dec::Abstype(_, _, _) => st.err(dec, ErrorKind::Unsupported("`abstype` declarations")),
    // @def(20)
    sml_hir::Dec::Exception(ex_binds) => {
      let mut val_env = ValEnv::default();
      for ex_bind in ex_binds {
        match ex_bind {
          // @def(30)
          sml_hir::ExBind::New(name, param) => {
            let mut ty = Ty::EXN;
            let param = param.map(|param| ty::get(st, cx, ars, ty::Mode::Regular, param));
            if let Some(param) = param {
              ty = st.syms_tys.tys.fun(param, ty);
            }
            let exn = st.syms_tys.syms.insert_exn(st.mk_path(name.clone()), param);
            let vi = ValInfo {
              ty_scheme: TyScheme::zero(ty),
              id_status: IdStatus::Exn(exn),
              defs: st.def(dec.into()).into_iter().collect(),
              disallow: None,
            };
            if let Some(e) = ins_check_name(&mut val_env, name.clone(), vi, Item::Val) {
              st.err(dec, e);
            }
          }
          // @def(31)
          sml_hir::ExBind::Copy(name, path) => {
            let val_info = get_val_info(&cx.env, path);
            for e in val_info.disallow {
              st.err(dec, e.into());
            }
            let val_info = match val_info.val {
              Ok(x) => x,
              Err(e) => {
                st.err(dec, e.into());
                return;
              }
            };
            if let Some(d) = &val_info.disallow {
              st.err(dec, ErrorKind::Disallowed(Item::Val, d.clone(), path.last().clone()));
            }
            match val_info.id_status {
              IdStatus::Exn(_) => {
                match ins_no_dupe(&mut val_env, name.clone(), val_info.clone(), Item::Val) {
                  None => {
                    if let Some(&def) = val_info.defs.iter().next() {
                      st.info.entries.defs.dec.insert(dec, def);
                    }
                  }
                  Some(e) => st.err(dec, e),
                }
              }
              _ => st.err(dec, ErrorKind::ExnCopyNotExnIdStatus(path.clone())),
            }
          }
        }
      }
      env.val_env.append(&mut val_env);
    }
    // @def(21)
    sml_hir::Dec::Local(local_dec, in_dec) => {
      let mut local_env = Env::default();
      get(st, cfg, cx, ars, &mut local_env, local_dec);
      let mut cx = cx.clone();
      cx.env.append(&mut local_env);
      get(st, cfg, &cx, ars, env, in_dec);
    }
    // @def(22)
    sml_hir::Dec::Open(paths) => {
      for path in paths {
        let got_env = get_env(&cx.env, path.all_names());
        for e in got_env.disallow {
          st.err(dec, e.into());
        }
        match got_env.val {
          Ok(got_env) => env.append(&mut got_env.clone()),
          Err(e) => st.err(dec, e.into()),
        }
      }
    }
  }
}

fn get_pat_and_src_exp(
  st: &mut St<'_>,
  cfg: pat::Cfg,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  ve: &mut ValEnv,
  val_bind: &sml_hir::ValBind,
  src_exp: &mut FxHashMap<str_util::Name, sml_hir::ExpIdx>,
) -> (Pat, Ty) {
  // this makes the rank of the bindings from the pat in a `val` the same as the variables bound by
  // any fns on the exp, so we don't generalize a recursive call inside the exp, but we can
  // generalize outside.
  st.syms_tys.tys.inc_meta_var_rank();
  let ret = pat::get(st, cfg, ars, cx, ve, val_bind.pat);
  st.syms_tys.tys.dec_meta_var_rank();
  for (name, _) in ve.iter() {
    if !src_exp.contains_key(name) {
      src_exp.insert(name.clone(), val_bind.exp);
    }
  }
  ret
}

pub(crate) fn add_fixed_ty_vars(
  st: &mut St<'_>,
  idx: sml_hir::Idx,
  cx: &mut Cx,
  src: TyVarSrc,
  ty_vars: &[sml_hir::TyVar],
) -> generalize::FixedTyVars {
  let mut ret = generalize::FixedTyVars::default();
  for ty_var in ty_vars {
    let fv = st.syms_tys.tys.fixed_var(ty_var.clone(), src);
    if cx.fixed.insert(ty_var.clone(), fv).is_some() {
      let e = ErrorKind::Duplicate(Item::TyVar, ty_var.as_name().clone());
      st.err(idx, e);
    }
    ret.push(fv);
  }
  ret
}

fn get_ty_binds(
  st: &mut St<'_>,
  idx: sml_hir::Idx,
  cx: &mut Cx,
  ars: &sml_hir::Arenas,
  ty_env: &mut TyEnv,
  ty_binds: &[sml_hir::TyBind],
) {
  for ty_bind in ty_binds {
    let fixed = add_fixed_ty_vars(st, idx, cx, TyVarSrc::Ty, &ty_bind.ty_vars);
    let ty = ty::get(st, cx, ars, ty::Mode::TyRhs, ty_bind.ty);
    let ty_scheme = generalize::get_fixed(&mut st.syms_tys.tys, fixed, ty);
    let ty_info =
      TyInfo { ty_scheme, val_env: ValEnv::default(), def: st.def(idx), disallow: None };
    if let Some(e) = ins_no_dupe(ty_env, ty_bind.name.clone(), ty_info, Item::Ty) {
      st.err(idx, e);
    }
    for ty_var in &ty_bind.ty_vars {
      cx.fixed.remove(ty_var);
    }
  }
}

struct Datatype {
  started: StartedSym,
  fixed: generalize::FixedTyVars,
  out_ty: Ty,
  ty_scheme: TyScheme,
}

pub(crate) fn get_dat_binds(
  st: &mut St<'_>,
  idx: sml_hir::Idx,
  mut cx: Cx,
  ars: &sml_hir::Arenas,
  dat_binds: &[sml_hir::DatBind],
  with_types: &[sml_hir::TyBind],
) -> (TyEnv, ValEnv) {
  // 'fake' because it's just to allow recursive reference, it doesn't have the val env filled in
  // with the constructors.
  let mut fake_ty_env = TyEnv::default();
  let mut datatypes = Vec::<Datatype>::new();
  // do a first pass through the datatypes to allow for recursive reference, and to put them in
  // scope for the types.
  for dat_bind in dat_binds {
    let started = st.syms_tys.syms.start(st.mk_path(dat_bind.name.clone()));
    // just create the fixed ty vars, do not bring them into the scope of the cx yet.
    let mut fixed = generalize::FixedTyVars::default();
    for ty_var in &dat_bind.ty_vars {
      let fv = st.syms_tys.tys.fixed_var(ty_var.clone(), TyVarSrc::Ty);
      fixed.push(fv);
    }
    let out_args: Vec<_> = fixed.iter().collect();
    let out_ty = st.syms_tys.tys.con(out_args, started.sym());
    // just `get` would also work, because `out_ty` mentions every fixed var.
    let ty_scheme = generalize::get_fixed(&mut st.syms_tys.tys, fixed.clone(), out_ty);
    let ty_info = TyInfo {
      ty_scheme: ty_scheme.clone(),
      val_env: ValEnv::default(),
      def: st.def(idx),
      disallow: None,
    };
    if let Some(e) = ins_no_dupe(&mut fake_ty_env, dat_bind.name.clone(), ty_info, Item::Ty) {
      st.err(idx, e);
    }
    datatypes.push(Datatype { started, fixed, out_ty, ty_scheme });
  }
  // bring all the datatypes into scope.
  cx.env.ty_env.append(&mut fake_ty_env.clone());
  // now get the `withtype`s. this `ty_env` will be the ultimate one we return.
  let mut ty_env = TyEnv::default();
  get_ty_binds(st, idx, &mut cx, ars, &mut ty_env, with_types);
  // make sure the types did not conflict with the datatypes.
  for (name, val) in ty_env.iter() {
    if let Some(e) = ins_no_dupe(&mut fake_ty_env, name.clone(), val.clone(), Item::Ty) {
      st.err(idx, e);
    }
  }
  // bring all the types into scope.
  cx.env.ty_env.append(&mut ty_env.clone());
  // datatypes and types are now in scope. now get the datatype constructors.
  let mut big_val_env = ValEnv::default();
  assert_eq!(
    dat_binds.len(),
    datatypes.len(),
    "we created datatypes from a for loop over dat_binds"
  );
  // @def(28), @def(81)
  for (dat_bind, datatype) in dat_binds.iter().zip(datatypes) {
    // bring the type variables for this datatype into scope.
    for fv in datatype.fixed.iter() {
      let ty_var = match st.syms_tys.tys.data(fv) {
        TyData::FixedVar(fv) => fv.ty_var.clone(),
        _ => unreachable!("not a fixed var"),
      };
      if cx.fixed.insert(ty_var.clone(), fv).is_some() {
        let e = ErrorKind::Duplicate(Item::TyVar, ty_var.as_name().clone());
        st.err(idx, e);
      }
    }
    let mut val_env = SymValEnv::default();
    // @def(29), @def(82)
    for con_bind in &dat_bind.cons {
      let mut ty = datatype.out_ty;
      if let Some(of_ty) = con_bind.ty {
        let param = ty::get(st, &cx, ars, ty::Mode::TyRhs, of_ty);
        ty = st.syms_tys.tys.fun(param, ty);
      };
      // just `get` would also work, because `ty_scheme` contains `out_ty`, which mentions every
      // fixed var.
      let ty_scheme = generalize::get_fixed(&mut st.syms_tys.tys, datatype.fixed.clone(), ty);
      let vi = ValInfo {
        ty_scheme,
        id_status: IdStatus::Con,
        defs: st.def(idx).into_iter().collect(),
        disallow: None,
      };
      let e = check_name(&con_bind.name).or_else(|| {
        (!val_env.insert(con_bind.name.clone(), vi))
          .then(|| ErrorKind::Duplicate(Item::Val, con_bind.name.clone()))
      });
      if let Some(e) = e {
        st.err(idx, e);
      }
    }
    // NOTE: no checking for duplicates here
    big_val_env.append(&mut val_env.clone().into());
    let ty_info =
      TyInfo { ty_scheme: datatype.ty_scheme, val_env, def: st.def(idx), disallow: None };
    let ty_info_dve = ty_info.clone().with_default_val_env_type();
    let equality = equality::get_ty_info(
      st.info.mode,
      &st.syms_tys.syms,
      &mut st.syms_tys.tys,
      ty_info_dve.clone(),
    );
    let equality = match equality {
      Ok(()) => Equality::Sometimes,
      Err(_) => Equality::Never,
    };
    st.syms_tys.syms.finish(datatype.started, ty_info, equality);
    ty_env.insert(dat_bind.name.clone(), ty_info_dve);
    for ty_var in &dat_bind.ty_vars {
      cx.fixed.remove(ty_var);
    }
  }
  (ty_env, big_val_env)
}

fn expansive(cx: &Cx, ars: &sml_hir::Arenas, exp: sml_hir::ExpIdx) -> bool {
  let Some(exp) = exp else { return false };
  match &ars.exp[exp] {
    sml_hir::Exp::Hole | sml_hir::Exp::SCon(_) | sml_hir::Exp::Path(_) | sml_hir::Exp::Fn(_, _) => {
      false
    }
    sml_hir::Exp::Let(_, _) | sml_hir::Exp::Raise(_) | sml_hir::Exp::Handle(_, _) => true,
    sml_hir::Exp::Record(rows) => rows.iter().any(|&(_, exp)| expansive(cx, ars, exp)),
    sml_hir::Exp::App(func, argument) => {
      !constructor(cx, ars, *func) || expansive(cx, ars, *argument)
    }
    sml_hir::Exp::Typed(exp, _, _) => expansive(cx, ars, *exp),
    sml_hir::Exp::Vector(exps) => exps.iter().any(|&exp| expansive(cx, ars, exp)),
  }
}

/// this will sometimes return true for expressions that could not possibly be a "constructor" in
/// the sense of section 4.7 of the definition, like records. but there would have been a type error
/// emitted for the application already anyway.
fn constructor(cx: &Cx, ars: &sml_hir::Arenas, exp: sml_hir::ExpIdx) -> bool {
  let Some(exp) = exp else { return true };
  match &ars.exp[exp] {
    sml_hir::Exp::Hole | sml_hir::Exp::SCon(_) => true,
    sml_hir::Exp::Let(_, _)
    | sml_hir::Exp::App(_, _)
    | sml_hir::Exp::Handle(_, _)
    | sml_hir::Exp::Raise(_)
    | sml_hir::Exp::Fn(_, _)
    | sml_hir::Exp::Vector(_) => false,
    sml_hir::Exp::Record(rows) => rows.iter().any(|&(_, exp)| constructor(cx, ars, exp)),
    sml_hir::Exp::Typed(exp, _, _) => constructor(cx, ars, *exp),
    sml_hir::Exp::Path(path) => {
      if path.prefix().is_empty() && path.last().as_str() == "ref" {
        return false;
      }
      let val_info =
        get_env_raw(&cx.env, path.prefix()).ok().and_then(|env| env.val_env.get(path.last()));
      val_info.map_or(true, |x| matches!(x.id_status, IdStatus::Con | IdStatus::Exn(_)))
    }
  }
}

pub(crate) fn maybe_effectful(ars: &sml_hir::Arenas, decs: &[sml_hir::DecIdx]) -> bool {
  decs.iter().any(|&dec| maybe_effectful_one(ars, dec))
}

pub(crate) fn maybe_effectful_one(ars: &sml_hir::Arenas, dec: sml_hir::DecIdx) -> bool {
  match &ars.dec[dec] {
    sml_hir::Dec::Val(_, val_binds, _) => maybe_effectful_val(ars, val_binds),
    sml_hir::Dec::Ty(_)
    | sml_hir::Dec::Datatype(_, _)
    | sml_hir::Dec::DatatypeCopy(_, _)
    | sml_hir::Dec::Exception(_)
    | sml_hir::Dec::Open(_) => false,
    sml_hir::Dec::Abstype(_, _, dec) => maybe_effectful(ars, dec),
    sml_hir::Dec::Local(fst, snd) => maybe_effectful(ars, fst) || maybe_effectful(ars, snd),
  }
}

fn maybe_effectful_val(ars: &sml_hir::Arenas, val_binds: &[sml_hir::ValBind]) -> bool {
  val_binds.iter().any(|val_bind| {
    exp::maybe_effectful(ars, val_bind.exp) || pat::maybe_refutable(ars, val_bind.pat)
  })
}

/// "maybe" because it's more liberal (i.e. if it's a hole or not there at all, return true).
fn maybe_fn(ar: &sml_hir::ExpArena, exp: sml_hir::ExpIdx) -> bool {
  let Some(exp) = exp else { return true };
  match &ar[exp] {
    sml_hir::Exp::Hole | sml_hir::Exp::Fn(_, _) => true,
    sml_hir::Exp::SCon(_)
    | sml_hir::Exp::Path(_)
    | sml_hir::Exp::Record(_)
    | sml_hir::Exp::Let(_, _)
    | sml_hir::Exp::App(_, _)
    | sml_hir::Exp::Handle(_, _)
    | sml_hir::Exp::Raise(_)
    | sml_hir::Exp::Vector(_) => false,
    sml_hir::Exp::Typed(exp, _, _) => maybe_fn(ar, *exp),
  }
}
