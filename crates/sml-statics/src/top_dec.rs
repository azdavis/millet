//! Checking top-level declarations (and therefore signatures, and therefore specifications).

mod enrich;
mod env_syms;
mod instance;
mod realize;
mod sharing_ty;
mod ty_con_paths;
mod where_ty;

use crate::error::{ErrorKind, FunctorSugarUser};
use crate::get_env::{get_env, get_ty_info};
use crate::util::{ins_check_name, ins_no_dupe};
use crate::{basis::Bs, config::Cfg, dec, st::St, ty};
use fast_hash::FxHashSet;
use sml_statics_types::env::{Env, FunEnv, FunSig, Sig, SigEnv, StrEnv, TyNameSet};
use sml_statics_types::info::{IdStatus, TyEnv, TyInfo, ValInfo};
use sml_statics_types::sym::{Equality, StartedSym, SymTyInfo, SymValEnv, SymsMarker};
use sml_statics_types::ty::{BoundTyVarData, Ty, TyData, TyScheme, TyVarSrc, Tys};
use sml_statics_types::{generalize, item::Item, mode::Mode, overload, util::n_ary_con};

pub(crate) fn get(
  st: &mut St<'_>,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  root: &[sml_hir::StrDecIdx],
) -> Bs {
  let mut ac = Bs::default();
  get_str_dec(st, bs, ars, StrDecAc::Bs(&mut ac), root);
  Bs { fun_env: ac.fun_env, sig_env: ac.sig_env, env: ac.env }
}

enum StrDecAc<'a> {
  Env(&'a mut Env),
  Bs(&'a mut Bs),
}

impl StrDecAc<'_> {
  fn as_mut_env(&mut self) -> &mut Env {
    match self {
      StrDecAc::Env(env) => env,
      StrDecAc::Bs(bs) => &mut bs.env,
    }
  }
}

fn get_str_dec(
  st: &mut St<'_>,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  ac: StrDecAc<'_>,
  str_decs: &[sml_hir::StrDecIdx],
) {
  match str_decs[..] {
    [] => return,
    [x] => {
      get_str_dec_one(st, bs, ars, ac, x);
      return;
    }
    _ => {}
  }
  // @def(59), @def(60)
  let mut bs = bs.clone();
  match ac {
    StrDecAc::Env(ac) => {
      let mut one_env = Env::default();
      for &str_dec in str_decs {
        get_str_dec_one(st, &bs, ars, StrDecAc::Env(&mut one_env), str_dec);
        bs.env.append(&mut one_env.clone());
        ac.append(&mut one_env);
      }
    }
    StrDecAc::Bs(ac) => {
      for &str_dec in str_decs {
        let mut one_bs = Bs::default();
        get_str_dec_one(st, &bs, ars, StrDecAc::Bs(&mut one_bs), str_dec);
        bs.append(one_bs.clone());
        ac.append(one_bs);
      }
    }
  }
}

fn get_str_dec_one(
  st: &mut St<'_>,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  mut ac: StrDecAc<'_>,
  str_dec: sml_hir::StrDecIdx,
) {
  match &ars.str_dec[str_dec] {
    // @def(56)
    sml_hir::StrDec::Dec(dec) => {
      dec::get(st, Cfg::default(), &bs.as_cx(), ars, ac.as_mut_env(), dec);
    }
    // @def(57)
    sml_hir::StrDec::Structure(str_binds) => {
      // @def(61)
      let mut str_env = StrEnv::default();
      for str_bind in str_binds {
        let mut env = Env::new(st.def(str_dec.into()));
        st.push_prefix(str_bind.name.clone());
        get_str_exp(st, bs, ars, &mut env, str_bind.str_exp);
        st.pop_prefix();
        if let Some(e) = ins_no_dupe(&mut str_env, str_bind.name.clone(), env, Item::Struct) {
          st.err(str_dec, e);
        }
      }
      ac.as_mut_env().str_env.append(&mut str_env);
    }
    // @def(66), @def(88)
    sml_hir::StrDec::Signature(sig_binds) => {
      let ac = match ac {
        StrDecAc::Bs(ac) => ac,
        StrDecAc::Env(_) => {
          st.err(str_dec, ErrorKind::DecNotAllowedHere);
          return;
        }
      };
      let mut sig_env = SigEnv::default();
      // @def(67)
      for sig_bind in sig_binds {
        let marker = st.syms_tys.syms.mark();
        let mut env = Env::new(st.def(str_dec.into()));
        st.push_prefix(sig_bind.name.clone());
        get_sig_exp(st, bs, ars, &mut env, sig_bind.sig_exp);
        st.pop_prefix();
        let sig = env_to_sig(&st.syms_tys.tys, env, marker);
        if let Some(e) = ins_no_dupe(&mut sig_env, sig_bind.name.clone(), sig, Item::Sig) {
          st.err(str_dec, e);
        }
      }
      ac.sig_env.append(&mut sig_env);
    }
    // @def(85), @def(89)
    sml_hir::StrDec::Functor(fun_binds) => {
      let ac = match ac {
        StrDecAc::Bs(ac) => ac,
        StrDecAc::Env(_) => {
          st.err(str_dec, ErrorKind::DecNotAllowedHere);
          return;
        }
      };
      let mut fun_env = FunEnv::default();
      // @def(86)
      for fun_bind in fun_binds {
        let mut param_env = Env::default();
        let marker = st.syms_tys.syms.mark();
        get_sig_exp(st, bs, ars, &mut param_env, fun_bind.param_sig);
        let param_sig = env_to_sig(&st.syms_tys.tys, param_env, marker);
        let mut bs_clone = bs.clone();
        bs_clone.env.str_env.insert(fun_bind.param_name.clone(), param_sig.env.clone());
        let mut body_env = Env::new(st.def(str_dec.into()));
        let marker = st.syms_tys.syms.mark();
        st.push_prefix(str_util::Name::new(format!("{}(...)", fun_bind.functor_name)));
        get_str_exp(st, &bs_clone, ars, &mut body_env, fun_bind.body);
        st.pop_prefix();
        let mut body_ty_names = TyNameSet::default();
        env_syms::get(&st.syms_tys.tys, &body_env, &mut |x| {
          if x.generated_after(marker) {
            body_ty_names.insert(x);
          }
        });
        for sym in &param_sig.ty_names {
          body_ty_names.remove(sym);
        }
        let fun_name = fun_bind.functor_name.clone();
        let fun_sig = FunSig {
          param: param_sig,
          body_ty_names,
          body_env,
          flavor: fun_bind.flavor,
          disallow: None,
        };
        if let Some(e) = ins_no_dupe(&mut fun_env, fun_name, fun_sig, Item::Functor) {
          st.err(str_dec, e);
        }
      }
      ac.fun_env.append(&mut fun_env);
    }
    // @def(58)
    sml_hir::StrDec::Local(local_dec, in_dec) => {
      let mut local_bs = Bs::default();
      get_str_dec(st, bs, ars, StrDecAc::Bs(&mut local_bs), local_dec);
      let mut bs = bs.clone();
      bs.append(local_bs);
      get_str_dec(st, &bs, ars, ac, in_dec);
    }
  }
}

fn get_str_exp(
  st: &mut St<'_>,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  ac: &mut Env,
  str_exp: sml_hir::StrExpIdx,
) {
  let Some(str_exp) = str_exp else { return };
  match &ars.str_exp[str_exp] {
    // @def(50)
    sml_hir::StrExp::Struct(str_dec) => get_str_dec(st, bs, ars, StrDecAc::Env(ac), str_dec),
    // @def(51)
    sml_hir::StrExp::Path(path) => {
      let got_env = get_env(&bs.env, path.all_names());
      for e in got_env.disallow {
        st.err(str_exp, e.into());
      }
      match got_env.val {
        Ok(got_env) => {
          let defs = st.info.entries.defs.str_exp.entry(str_exp).or_default();
          defs.extend(got_env.defs.iter().copied());
          ac.append(&mut got_env.clone());
        }
        Err(e) => st.err(str_exp, e.into()),
      }
    }
    // @def(52), @def(53)
    sml_hir::StrExp::Ascription(inner_str_exp, asc, sig_exp) => {
      let mut str_exp_env = Env::default();
      get_str_exp(st, bs, ars, &mut str_exp_env, *inner_str_exp);
      let mut sig_exp_env = Env::default();
      let marker = st.syms_tys.syms.mark();
      let ov = get_sig_exp(st, bs, ars, &mut sig_exp_env, *sig_exp);
      let sig = env_to_sig(&st.syms_tys.tys, sig_exp_env, marker);
      let mut subst = realize::TyRealization::default();
      let mut to_add = sig.env.clone();
      let idx = sml_hir::Idx::from(str_exp);
      match st.info.mode {
        Mode::Regular(_) | Mode::Dynamics => {
          instance::env_of_sig(st, idx, &mut subst, &str_exp_env, &sig);
          realize::get_env(&mut st.syms_tys.tys, &subst, &mut to_add);
          enrich::get_env(st, idx, &str_exp_env, &to_add);
        }
        Mode::BuiltinLib(_) => {
          // @test(hover::doc::std_basis_structure)
          //
          // this changes the def for `struct end :> SIG` or `struct end : SIG`, specifically in
          // builtin lib files, which is a pattern we use to "conjure up" structures that ascribe to
          // the sig.
          //
          // this, in conjunction with some extra hackery for "name" signature expressions (like
          // `SIG`) in builtin lib files, lets us get the docs on the signature for that "conjured"
          // structure as well.
          if let Some(sml_hir::StrExp::Struct(decs)) = inner_str_exp.map(|x| &ars.str_exp[x]) {
            if let Some(sml_hir::SigExp::Name(_)) = sig_exp.map(|x| &ars.sig_exp[x]) {
              if decs.is_empty() {
                ac.defs.extend(to_add.defs.iter().copied());
              }
            }
          }
        }
        Mode::PathOrder => {}
      }
      if matches!(asc, sml_hir::Ascription::Opaque) {
        subst.clear();
        gen_fresh_syms(st, &mut subst, &sig.ty_names);
        to_add = sig.env.clone();
        realize::get_env(&mut st.syms_tys.tys, &subst, &mut to_add);
      }
      enrich_defs(&str_exp_env, &mut to_add);
      if let Some(ov) = ov {
        let ty_info = to_add.ty_env.get(ov.as_str()).expect("should get overloaded ty");
        match st.syms_tys.tys.data(ty_info.ty_scheme.ty) {
          TyData::Con(data) => {
            assert!(data.args.is_empty());
            st.syms_tys.syms.overloads_mut()[ov].push(data.sym);
          }
          t => unreachable!("overload not a Con: {t:?}"),
        }
      }
      ac.append(&mut to_add);
    }
    // @def(54)
    sml_hir::StrExp::App(fun_name, arg_str_exp, flavor) => {
      let Some(fun_sig) = bs.fun_env.get(fun_name) else {
        st.err(str_exp, ErrorKind::Undefined(Item::Functor, fun_name.clone()));
        return;
      };
      if let Some(d) = &fun_sig.disallow {
        st.err(str_exp, ErrorKind::Disallowed(Item::Functor, d.clone(), fun_name.clone()));
      }
      let idx = sml_hir::Idx::from(str_exp);
      let sugar_user = match (fun_sig.flavor, flavor) {
        (sml_hir::Flavor::Plain, sml_hir::Flavor::Plain)
        | (sml_hir::Flavor::Sugared, sml_hir::Flavor::Sugared) => None,
        (sml_hir::Flavor::Plain, sml_hir::Flavor::Sugared) => Some(FunctorSugarUser::App),
        (sml_hir::Flavor::Sugared, sml_hir::Flavor::Plain) => Some(FunctorSugarUser::Def),
      };
      if let Some(sugar_user) = sugar_user {
        st.err(str_exp, ErrorKind::MismatchedFunctorSugar(sugar_user));
      }
      let mut arg_env = Env::default();
      get_str_exp(st, bs, ars, &mut arg_env, *arg_str_exp);
      let mut subst = realize::TyRealization::default();
      let mut to_add = fun_sig.body_env.clone();
      let arg_idx = sml_hir::Idx::from(arg_str_exp.unwrap_or(str_exp));
      instance::env_of_sig(st, arg_idx, &mut subst, &arg_env, &fun_sig.param);
      gen_fresh_syms(st, &mut subst, &fun_sig.body_ty_names);
      realize::get_env(&mut st.syms_tys.tys, &subst, &mut to_add);
      let mut param_env = fun_sig.param.env.clone();
      realize::get_env(&mut st.syms_tys.tys, &subst, &mut param_env);
      enrich::get_env(st, arg_idx, &arg_env, &param_env);
      let def = st.def_opt(idx);
      for (_, env) in to_add.str_env.iter_mut() {
        env.defs.extend(def.iter().copied());
      }
      for (_, ty_env) in to_add.ty_env.iter_mut() {
        ty_env.defs.extend(def.iter().copied());
      }
      for (_, val_info) in to_add.val_env.iter_mut() {
        val_info.defs.extend(def.iter().copied());
      }
      let str_exp_defs = st.info.entries.defs.str_exp.entry(str_exp).or_default();
      str_exp_defs.extend(fun_sig.body_env.defs.iter().copied());
      ac.append(&mut to_add);
    }
    // @def(55)
    sml_hir::StrExp::Let(str_dec, str_exp) => {
      let mut let_env = Env::default();
      get_str_dec(st, bs, ars, StrDecAc::Env(&mut let_env), str_dec);
      let mut bs = bs.clone();
      bs.env.append(&mut let_env);
      get_str_exp(st, &bs, ars, ac, *str_exp);
    }
  }
}

/// helps get the defs (and therefore e.g. docs) from a structure that ascribes to a signature, not
/// just the docs from the signature
fn enrich_defs(general: &Env, specific: &mut Env) {
  specific.defs.extend(general.defs.iter().copied());
  for (name, specific) in specific.str_env.iter_mut() {
    let Some(general) = general.str_env.get(name) else { continue };
    enrich_defs(general, specific);
  }
  for (name, specific) in specific.ty_env.iter_mut() {
    let Some(general) = general.ty_env.get(name) else { continue };
    specific.defs.extend(general.defs.iter().copied());
    for (name, specific) in specific.val_env.iter_mut() {
      let Some(general) = general.val_env.get(name) else { continue };
      specific.defs.extend(general.defs.iter().copied());
    }
  }
  for (name, specific) in specific.val_env.iter_mut() {
    let Some(general) = general.val_env.get(name) else { continue };
    specific.defs.extend(general.defs.iter().copied());
  }
}

fn get_sig_exp(
  st: &mut St<'_>,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  ac: &mut Env,
  sig_exp: sml_hir::SigExpIdx,
) -> Option<overload::Basic> {
  let sig_exp = sig_exp?;
  match &ars.sig_exp[sig_exp] {
    // @def(62)
    sml_hir::SigExp::Spec(spec) => {
      get_spec(st, bs, ars, ac, spec);
      None
    }
    // @def(63)
    sml_hir::SigExp::Name(name) => {
      let Some(sig) = bs.sig_env.get(name) else {
        st.err(sig_exp, ErrorKind::Undefined(Item::Sig, name.clone()));
        return None;
      };
      if let Some(d) = &sig.disallow {
        st.err(sig_exp, ErrorKind::Disallowed(Item::Sig, d.clone(), name.clone()));
      }
      let mut subst = realize::TyRealization::default();
      gen_fresh_syms(st, &mut subst, &sig.ty_names);
      let mut sig_env = sig.env.clone();
      realize::get_env(&mut st.syms_tys.tys, &subst, &mut sig_env);
      let sig_exp_defs = st.info.entries.defs.sig_exp.entry(sig_exp).or_default();
      sig_exp_defs.extend(sig.env.defs.iter().copied());
      // @test(hover::doc::std_basis_structure)
      if matches!(st.info.mode, Mode::BuiltinLib(_)) {
        ac.defs.extend(sig.env.defs.iter().copied());
      }
      ac.append(&mut sig_env);
      match st.info.mode {
        Mode::BuiltinLib(_) => match name.as_str() {
          "WORD" => Some(overload::Basic::Word),
          "INTEGER" | "INT_INF" => Some(overload::Basic::Int),
          "REAL" => Some(overload::Basic::Real),
          _ => None,
        },
        Mode::Regular(_) | Mode::PathOrder | Mode::Dynamics => None,
      }
    }
    // @def(64)
    sml_hir::SigExp::Where(inner, kind) => {
      let marker = st.syms_tys.syms.mark();
      let mut inner_env = Env::default();
      let ov = get_sig_exp(st, bs, ars, &mut inner_env, *inner);
      where_ty::get(st, sig_exp.into(), bs, marker, ars, &mut inner_env, kind);
      ac.append(&mut inner_env);
      ov
    }
  }
}

fn gen_fresh_syms(st: &mut St<'_>, subst: &mut realize::TyRealization, ty_names: &TyNameSet) {
  let mut ac = Vec::<(StartedSym, SymTyInfo, Equality)>::new();
  for &sym in ty_names {
    let sym_info = st.syms_tys.syms.get(sym).unwrap();
    let mut ty_info = sym_info.ty_info.clone();
    let equality = sym_info.equality;
    let started = st.syms_tys.syms.start(st.mk_path(sym_info.path.last().clone()));
    let bound_vars = ty_info.ty_scheme.bound_vars.clone();
    let ty_scheme = n_ary_con(&mut st.syms_tys.tys, bound_vars, started.sym());
    ty_info.ty_scheme = ty_scheme.clone();
    ac.push((started, ty_info, equality));
    subst.insert(sym, ty_scheme);
  }
  for (started, mut ty_info, equality) in ac {
    realize::get_sym_val_env(&mut st.syms_tys.tys, subst, &mut ty_info.val_env);
    st.syms_tys.syms.finish(started, ty_info, equality);
  }
}

// @def(65)
fn env_to_sig(tys: &Tys, env: Env, marker: SymsMarker) -> Sig {
  let mut ty_names = TyNameSet::default();
  env_syms::get(tys, &env, &mut |x| {
    if x.generated_after(marker) {
      ty_names.insert(x);
    }
  });
  Sig { ty_names, env, disallow: None }
}

fn get_spec(
  st: &mut St<'_>,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  ac: &mut Env,
  specs: &[sml_hir::SpecIdx],
) {
  match specs[..] {
    [] => return,
    [x] => {
      get_spec_one(st, bs, ars, ac, x);
      return;
    }
    _ => {}
  }
  // @def(76), @def(77)
  let mut bs = bs.clone();
  let mut one_env = Env::default();
  for &seq_spec in specs {
    get_spec_one(st, &bs, ars, &mut one_env, seq_spec);
    bs.env.append(&mut one_env.clone());
    append_no_dupe(st, seq_spec.into(), ac, &mut one_env);
  }
}

fn get_spec_one(
  st: &mut St<'_>,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  ac: &mut Env,
  spec: sml_hir::SpecIdx,
) {
  match &ars.spec[spec] {
    // @def(68)
    sml_hir::Spec::Val(ty_vars, val_descs) => {
      // @def(79)
      let mut cx = bs.as_cx();
      let fixed = dec::add_fixed_ty_vars(st, spec.into(), &mut cx, TyVarSrc::Val, ty_vars);
      for val_desc in val_descs {
        let ty = ty::get(st, &cx, ars, ty::Mode::Regular, val_desc.ty);
        let ty_scheme = generalize::get(&mut st.syms_tys.tys, fixed.clone(), ty)
          .expect("should be no record meta vars because no patterns in specs");
        let vi = ValInfo {
          ty_scheme,
          id_status: IdStatus::Val,
          defs: st.def(spec.into()).into_iter().collect(),
          disallow: None,
        };
        let name = &val_desc.name;
        if let Some(e) = ins_check_name(&mut ac.val_env, name.clone(), vi, Item::Val) {
          st.err(spec, e);
        }
      }
    }
    // @def(69)
    sml_hir::Spec::Ty(ty_descs) => {
      get_ty_desc(st, spec.into(), &mut ac.ty_env, ty_descs, Equality::Never);
    }
    // @def(70)
    sml_hir::Spec::EqTy(ty_descs) => {
      get_ty_desc(st, spec.into(), &mut ac.ty_env, ty_descs, Equality::Sometimes);
    }
    // @def(71)
    sml_hir::Spec::Datatype(dat_descs, with_types) => {
      let (ty_env, big_val_env) =
        dec::get_dat_binds(st, spec.into(), bs.as_cx(), ars, dat_descs, with_types);
      for (name, val) in ty_env {
        if let Some(e) = ins_no_dupe(&mut ac.ty_env, name, val, Item::Ty) {
          st.err(spec, e);
        }
      }
      for (name, val) in big_val_env {
        if let Some(e) = ins_no_dupe(&mut ac.val_env, name, val, Item::Val) {
          st.err(spec, e);
        }
      }
    }
    // @def(72)
    sml_hir::Spec::DatatypeCopy(name, path) => {
      let ty_info = get_ty_info(&bs.env, path);
      for e in ty_info.disallow {
        st.err(spec, e.into());
      }
      match ty_info.val {
        Ok(ty_info) => {
          let ins = ins_no_dupe(&mut ac.ty_env, name.clone(), ty_info.clone(), Item::Ty);
          if let Some(e) = ins {
            st.err(spec, e);
          }
          for (name, val_info) in ty_info.val_env.iter() {
            let ins = ins_no_dupe(&mut ac.val_env, name.clone(), val_info.clone(), Item::Val);
            if let Some(e) = ins {
              st.err(spec, e);
            }
          }
        }
        Err(e) => st.err(spec, e.into()),
      }
    }
    // @def(73), @def(83)
    sml_hir::Spec::Exception(ex_desc) => {
      let cx = bs.as_cx();
      // almost the same as the logic in dec::get, save for the check for ty vars.
      let mut ty = Ty::EXN;
      let param = ex_desc.ty.map(|param| ty::get(st, &cx, ars, ty::Mode::Regular, param));
      if let Some(param) = param {
        ty = st.syms_tys.tys.fun(param, ty);
      }
      let exn = st.syms_tys.syms.insert_exn(st.mk_path(ex_desc.name.clone()), param);
      let vi = ValInfo {
        ty_scheme: TyScheme::zero(ty),
        id_status: IdStatus::Exn(exn),
        defs: st.def(spec.into()).into_iter().collect(),
        disallow: None,
      };
      if let Some(e) = ins_check_name(&mut ac.val_env, ex_desc.name.clone(), vi, Item::Val) {
        st.err(spec, e);
      }
    }
    // @def(74), @def(84)
    sml_hir::Spec::Str(str_desc) => {
      let mut one_env = Env::default();
      st.push_prefix(str_desc.name.clone());
      get_sig_exp(st, bs, ars, &mut one_env, str_desc.sig_exp);
      st.pop_prefix();
      let name = str_desc.name.clone();
      if let Some(e) = ins_no_dupe(&mut ac.str_env, name, one_env, Item::Struct) {
        st.err(spec, e);
      }
    }
    // @def(75)
    sml_hir::Spec::Include(sig_exp) => {
      get_sig_exp(st, bs, ars, ac, *sig_exp);
    }
    // @def(78)
    sml_hir::Spec::Sharing(inner, kind, paths) => {
      let marker = st.syms_tys.syms.mark();
      let mut inner_env = Env::default();
      get_spec(st, bs, ars, &mut inner_env, inner);
      match kind {
        sml_hir::SharingKind::Regular => {
          sharing_ty::get(st, spec.into(), marker, &mut inner_env, paths);
        }
        sml_hir::SharingKind::Derived => {
          let mut all: Vec<_> = paths
            .iter()
            .filter_map(|path| {
              let ty_con_paths = ty_con_paths::get(&inner_env, path);
              for e in ty_con_paths.disallow {
                st.err(spec, e.into());
              }
              match ty_con_paths.val {
                Ok(x) => Some((path, x)),
                Err(e) => {
                  st.err(spec, e.into());
                  None
                }
              }
            })
            .collect();
          while let Some((struct_1, ty_cons_1)) = all.pop() {
            for ty_con in ty_cons_1 {
              for (struct_2, ty_cons_2) in &all {
                if !ty_cons_2.contains(&ty_con) {
                  continue;
                }
                let path_1 = ty_con_paths::join_paths(struct_1, &ty_con);
                let path_2 = ty_con_paths::join_paths(struct_2, &ty_con);
                sharing_ty::get(st, spec.into(), marker, &mut inner_env, &[path_1, path_2]);
              }
            }
          }
        }
      }
      append_no_dupe(st, spec.into(), ac, &mut inner_env);
    }
  }
}

/// empties other into ac, while checking for dupes.
fn append_no_dupe(st: &mut St<'_>, idx: sml_hir::Idx, ac: &mut Env, other: &mut Env) {
  for (name, val) in other.str_env.drain() {
    if let Some(e) = ins_no_dupe(&mut ac.str_env, name, val, Item::Struct) {
      st.err(idx, e);
    }
  }
  for (name, val) in other.ty_env.drain() {
    if let Some(e) = ins_no_dupe(&mut ac.ty_env, name, val, Item::Ty) {
      st.err(idx, e);
    }
  }
  for (name, val) in other.val_env.drain() {
    if let Some(e) = ins_no_dupe(&mut ac.val_env, name, val, Item::Val) {
      st.err(idx, e);
    }
  }
}

// @def(80)
fn get_ty_desc(
  st: &mut St<'_>,
  idx: sml_hir::Idx,
  ty_env: &mut TyEnv,
  ty_desc: &sml_hir::TyDesc,
  equality: Equality,
) {
  let mut ty_vars = FxHashSet::<&sml_hir::TyVar>::default();
  let started = st.syms_tys.syms.start(st.mk_path(ty_desc.name.clone()));
  for ty_var in &ty_desc.ty_vars {
    if !ty_vars.insert(ty_var) {
      let e = ErrorKind::Duplicate(Item::TyVar, ty_var.clone().into_name());
      st.err(idx, e);
    }
  }
  let bound_vars: Vec<_> =
    ty_desc.ty_vars.iter().map(|ty_var| BoundTyVarData::Named(ty_var.clone())).collect();
  let ty_info = TyInfo {
    ty_scheme: n_ary_con(&mut st.syms_tys.tys, bound_vars, started.sym()),
    val_env: SymValEnv::default(),
    defs: st.def(idx),
    disallow: None,
  };
  st.syms_tys.syms.finish(started, ty_info.clone(), equality);
  let e = ins_no_dupe(ty_env, ty_desc.name.clone(), ty_info.with_default_val_env_type(), Item::Ty);
  if let Some(e) = e {
    st.err(idx, e);
  }
}
