//! Checking declarations.

use crate::env::{Cx, Env};
use crate::error::{ErrorKind, Item};
use crate::generalize::{generalize, generalize_fixed, RecordMetaVar};
use crate::get_env::{get_env, get_ty_info, get_val_info};
use crate::types::{
  Equality, FixedTyVars, Generalizable, IdStatus, StartedSym, Ty, TyEnv, TyInfo, TyScheme,
  TyVarSrc, ValEnv, ValInfo,
};
use crate::util::{apply, ins_check_name, ins_no_dupe};
use crate::{config::Cfg, equality, exp, pat, pat_match::Pat, st::St, ty, unify::unify};
use fast_hash::{FxHashMap, FxHashSet};

pub(crate) fn get(
  st: &mut St,
  cfg: Cfg,
  cx: &Cx,
  ars: &sml_hir::Arenas,
  env: &mut Env,
  dec: sml_hir::DecIdx,
) {
  let dec = match dec {
    Some(x) => x,
    None => return,
  };
  match &ars.dec[dec] {
    // @def(15)
    sml_hir::Dec::Val(ty_vars, val_binds) => {
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
      let marker = st.syms.mark();
      while let Some(val_bind) = val_binds.get(idx) {
        if val_bind.rec {
          // this and all other remaining ones are recursive.
          break;
        }
        idx += 1;
        // @def(25)
        let (pm_pat, mut want) =
          get_pat_and_src_exp(st, pat_cfg, &cx, ars, &mut ve, val_bind, &mut src_exp);
        let got = exp::get_and_check_ty_escape(st, exp_cfg, &cx, marker, ars, val_bind.exp);
        unify(st, dec.into(), want.clone(), got);
        apply(&st.subst, &mut want);
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
      for (val_bind, (pm_pat, mut want)) in val_binds[idx..].iter().zip(got_pats) {
        // @def(26)
        if let Some(exp) = val_bind.exp {
          if !matches!(ars.exp[exp], sml_hir::Exp::Fn(_, _)) {
            st.err(dec, ErrorKind::ValRecExpNotFn);
          }
        }
        let got = exp::get_and_check_ty_escape(st, exp_cfg, &cx, marker, ars, val_bind.exp);
        unify(st, dec.into(), want.clone(), got);
        apply(&st.subst, &mut want);
        st.insert_bind(dec.into(), pm_pat, want);
      }
      let mut generalized = FxHashSet::<sml_hir::ExpIdx>::default();
      // generalize the entire merged ValEnv.
      for (name, val_info) in ve.iter_mut() {
        let &exp = src_exp.get(name).expect("should have an exp for every bound name");
        if !generalized.insert(exp) {
          continue;
        }
        let mv_g = st.meta_gen.generalizer();
        let g = generalize(mv_g, &st.subst, fixed.clone(), &mut val_info.ty_scheme);
        if expansive(&cx, ars, exp) && !val_info.ty_scheme.bound_vars.is_empty() {
          st.err(
            exp.map_or(sml_hir::Idx::Dec(dec), sml_hir::Idx::Exp),
            ErrorKind::BindPolymorphicExpansiveExp,
          );
        }
        if let Err(RecordMetaVar { idx, rows }) = g {
          st.err(idx, ErrorKind::UnresolvedRecordTy(rows));
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
      for e in ty_info.errors {
        st.err(dec, e);
      }
      if let Some(ty_info) = ty_info.val {
        env.ty_env.insert(name.clone(), ty_info.clone());
        env.val_env.append(&mut ty_info.val_env.clone());
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
            if let Some(ref param) = param {
              ty = Ty::fun(param.clone(), ty);
            }
            let exn = st.syms.insert_exn(st.mk_path(name.clone()), param);
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
            for e in val_info.errors {
              st.err(dec, e);
            }
            match val_info.val {
              Some(val_info) => match val_info.id_status {
                IdStatus::Exn(_) => {
                  match ins_no_dupe(&mut val_env, name.clone(), val_info.clone(), Item::Val) {
                    None => st.info.insert(dec.into(), None, val_info.defs.clone()),
                    Some(e) => st.err(dec, e),
                  }
                }
                _ => st.err(dec, ErrorKind::ExnCopyNotExnIdStatus(path.clone())),
              },
              None => st.err(dec, ErrorKind::Undefined(Item::Val, path.last().clone())),
            }
          }
        }
      }
      env.val_env.append(&mut val_env);
    }
    // @def(21)
    sml_hir::Dec::Local(local_dec, in_dec) => {
      let mut local_env = Env::default();
      get(st, cfg, cx, ars, &mut local_env, *local_dec);
      let mut cx = cx.clone();
      cx.env.append(&mut local_env);
      get(st, cfg, &cx, ars, env, *in_dec);
    }
    // @def(22)
    sml_hir::Dec::Open(paths) => {
      for path in paths {
        let got_env = get_env(&cx.env, path.all_names());
        for e in got_env.errors {
          st.err(dec, e);
        }
        if let Some(got_env) = got_env.val {
          env.append(&mut got_env.clone());
        }
      }
    }
    // @def(23), @def(24)
    sml_hir::Dec::Seq(decs) => {
      let mut cx = cx.clone();
      let mut one_env = Env::default();
      for &dec in decs {
        get(st, cfg, &cx, ars, &mut one_env, dec);
        cx.env.append(&mut one_env.clone());
        env.append(&mut one_env);
      }
    }
  }
}

fn get_pat_and_src_exp(
  st: &mut St,
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
  st.meta_gen.inc_rank();
  let ret = pat::get(st, cfg, ars, cx, ve, val_bind.pat);
  st.meta_gen.dec_rank();
  for (name, _) in ve.iter() {
    if !src_exp.contains_key(name) {
      src_exp.insert(name.clone(), val_bind.exp);
    }
  }
  ret
}

pub(crate) fn add_fixed_ty_vars(
  st: &mut St,
  idx: sml_hir::Idx,
  cx: &mut Cx,
  src: TyVarSrc,
  ty_vars: &[sml_hir::TyVar],
) -> FixedTyVars {
  let mut ret = FixedTyVars::default();
  for ty_var in ty_vars.iter() {
    let fv = st.fixed_gen.gen(ty_var.clone(), src);
    if cx.fixed.insert(ty_var.clone(), fv.clone()).is_some() {
      let e = ErrorKind::Duplicate(Item::TyVar, ty_var.as_name().clone());
      st.err(idx, e);
    }
    ret.insert(fv, None);
  }
  ret
}

fn get_ty_binds(
  st: &mut St,
  idx: sml_hir::Idx,
  cx: &mut Cx,
  ars: &sml_hir::Arenas,
  ty_env: &mut TyEnv,
  ty_binds: &[sml_hir::TyBind],
) {
  for ty_bind in ty_binds {
    let fixed = add_fixed_ty_vars(st, idx, cx, TyVarSrc::Ty, &ty_bind.ty_vars);
    let mut ty_scheme = TyScheme::zero(ty::get(st, cx, ars, ty::Mode::TyRhs, ty_bind.ty));
    generalize_fixed(fixed, &mut ty_scheme);
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
  fixed: FixedTyVars,
  out_ty: Ty,
  ty_scheme: TyScheme,
}

pub(crate) fn get_dat_binds(
  st: &mut St,
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
  for dat_bind in dat_binds.iter() {
    let started = st.syms.start(st.mk_path(dat_bind.name.clone()));
    // just create the fixed ty vars, do not bring them into the scope of the cx yet.
    let mut fixed = FixedTyVars::default();
    for ty_var in &dat_bind.ty_vars {
      fixed.insert(st.fixed_gen.gen(ty_var.clone(), TyVarSrc::Ty), None);
    }
    let out_ty = Ty::Con(fixed.keys().map(|x| Ty::FixedVar(x.clone())).collect(), started.sym());
    let ty_scheme = {
      let mut res = TyScheme::zero(out_ty.clone());
      // just `generalize` would also work, because `out_ty` mentions every fixed var.
      generalize_fixed(fixed.clone(), &mut res);
      res
    };
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
    for fv in datatype.fixed.keys() {
      if cx.fixed.insert(fv.ty_var().clone(), fv.clone()).is_some() {
        let e = ErrorKind::Duplicate(Item::TyVar, fv.ty_var().as_name().clone());
        st.err(idx, e);
      }
    }
    let mut val_env = ValEnv::default();
    // @def(29), @def(82)
    for con_bind in &dat_bind.cons {
      let mut ty = datatype.out_ty.clone();
      if let Some(of_ty) = con_bind.ty {
        ty = Ty::fun(ty::get(st, &cx, ars, ty::Mode::TyRhs, of_ty), ty);
      };
      let mut ty_scheme = TyScheme::zero(ty);
      // just `generalize` would also work, because `ty_scheme` contains `out_ty`, which mentions
      // every fixed var.
      generalize_fixed(datatype.fixed.clone(), &mut ty_scheme);
      let vi = ValInfo {
        ty_scheme,
        id_status: IdStatus::Con,
        defs: st.def(idx).into_iter().collect(),
        disallow: None,
      };
      if let Some(e) = ins_check_name(&mut val_env, con_bind.name.clone(), vi, Item::Val) {
        st.err(idx, e);
      }
    }
    // NOTE: no checking for duplicates here
    big_val_env.append(&mut val_env.clone());
    let ty_info =
      TyInfo { ty_scheme: datatype.ty_scheme, val_env, def: st.def(idx), disallow: None };
    let equality = match equality::get_ty_info(st, ty_info.clone()) {
      Ok(()) => Equality::Sometimes,
      Err(_) => Equality::Never,
    };
    st.syms.finish(datatype.started, ty_info.clone(), equality);
    ty_env.insert(dat_bind.name.clone(), ty_info);
    for ty_var in &dat_bind.ty_vars {
      cx.fixed.remove(ty_var);
    }
  }
  (ty_env, big_val_env)
}

fn expansive(cx: &Cx, ars: &sml_hir::Arenas, exp: sml_hir::ExpIdx) -> bool {
  let exp = match exp {
    Some(x) => x,
    None => return false,
  };
  match &ars.exp[exp] {
    sml_hir::Exp::Hole | sml_hir::Exp::SCon(_) | sml_hir::Exp::Path(_) | sml_hir::Exp::Fn(_, _) => {
      false
    }
    sml_hir::Exp::Let(_, _) | sml_hir::Exp::Raise(_) | sml_hir::Exp::Handle(_, _) => true,
    sml_hir::Exp::Record(rows) => rows.iter().any(|&(_, exp)| expansive(cx, ars, exp)),
    sml_hir::Exp::App(func, argument) => {
      !constructor(cx, ars, *func) || expansive(cx, ars, *argument)
    }
    sml_hir::Exp::Typed(exp, _) => expansive(cx, ars, *exp),
  }
}

/// this will sometimes return true for expressions that could not possibly be a "constructor" in
/// the sense of section 4.7 of the definition, like records. but there would have been a type error
/// emitted for the application already anyway.
fn constructor(cx: &Cx, ars: &sml_hir::Arenas, exp: sml_hir::ExpIdx) -> bool {
  let exp = match exp {
    Some(x) => x,
    None => return true,
  };
  match &ars.exp[exp] {
    sml_hir::Exp::Hole | sml_hir::Exp::SCon(_) => true,
    sml_hir::Exp::Let(_, _)
    | sml_hir::Exp::App(_, _)
    | sml_hir::Exp::Handle(_, _)
    | sml_hir::Exp::Raise(_)
    | sml_hir::Exp::Fn(_, _) => false,
    sml_hir::Exp::Record(rows) => rows.iter().any(|&(_, exp)| constructor(cx, ars, exp)),
    sml_hir::Exp::Typed(exp, _) => constructor(cx, ars, *exp),
    sml_hir::Exp::Path(path) => {
      if path.prefix().is_empty() && path.last().as_str() == "ref" {
        return false;
      }
      let val_info = get_val_info(&cx.env, path);
      val_info.val.map_or(true, |x| matches!(x.id_status, IdStatus::Con | IdStatus::Exn(_)))
    }
  }
}

pub(crate) fn maybe_effectful(ars: &sml_hir::Arenas, dec: sml_hir::DecIdx) -> bool {
  let dec = match dec {
    Some(x) => x,
    None => return true,
  };
  match &ars.dec[dec] {
    sml_hir::Dec::Val(_, val_binds) => maybe_effectful_val(ars, val_binds),
    sml_hir::Dec::Ty(_)
    | sml_hir::Dec::Datatype(_, _)
    | sml_hir::Dec::DatatypeCopy(_, _)
    | sml_hir::Dec::Exception(_)
    | sml_hir::Dec::Open(_) => false,
    sml_hir::Dec::Abstype(_, _, dec) => maybe_effectful(ars, *dec),
    sml_hir::Dec::Local(fst, snd) => maybe_effectful(ars, *fst) || maybe_effectful(ars, *snd),
    sml_hir::Dec::Seq(decs) => decs.iter().any(|&dec| maybe_effectful(ars, dec)),
  }
}

fn maybe_effectful_val(ars: &sml_hir::Arenas, val_binds: &[sml_hir::ValBind]) -> bool {
  val_binds.iter().any(|val_bind| {
    exp::maybe_effectful(ars, val_bind.exp) || pat::maybe_refutable(ars, val_bind.pat)
  })
}
