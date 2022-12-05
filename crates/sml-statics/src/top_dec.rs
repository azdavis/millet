//! Checking top-level declarations (and therefore signatures, and therefore specifications).

mod enrich;
mod env_syms;
mod realize;
mod sharing_ty;

use crate::compatible::eq_ty_fn_no_emit;
use crate::env::{Bs, Env, EnvLike, EnvStack, FunEnv, FunSig, Sig, SigEnv, StrEnv, TyNameSet};
use crate::error::{ErrorKind, FunctorSugarUser, Item};
use crate::generalize::{generalize, generalize_fixed};
use crate::get_env::{get_env_from_str_path, get_ty_info, get_ty_info_raw};
use crate::types::{
  BasicOverload, Equality, IdStatus, StartedSym, SymsMarker, Ty, TyEnv, TyInfo, TyScheme,
  TyVarKind, TyVarSrc, ValEnv, ValInfo,
};
use crate::util::{ins_check_name, ins_no_dupe};
use crate::{config::Cfg, dec, info::Mode, st::St, ty};
use fast_hash::{map, FxHashSet};

pub(crate) fn get(st: &mut St, bs: &Bs, ars: &sml_hir::Arenas, top_dec: sml_hir::StrDecIdx) -> Bs {
  let mut ac = Bs::<Env>::default();
  get_str_dec(st, bs, ars, StrDecAc::Bs(&mut ac), top_dec);
  Bs { fun_env: ac.fun_env, sig_env: ac.sig_env, env: EnvStack::one(ac.env) }
}

enum StrDecAc<'a> {
  Env(&'a mut Env),
  Bs(&'a mut Bs<Env>),
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
  st: &mut St,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  mut ac: StrDecAc<'_>,
  str_dec: sml_hir::StrDecIdx,
) {
  let str_dec = match str_dec {
    Some(x) => x,
    None => return,
  };
  match &ars.str_dec[str_dec] {
    // @def(56)
    sml_hir::StrDec::Dec(dec) => {
      dec::get(st, Cfg::default(), &bs.as_cx(), ars, ac.as_mut_env(), *dec);
    }
    // @def(57)
    sml_hir::StrDec::Structure(str_binds) => {
      // @def(61)
      let mut str_env = StrEnv::default();
      for str_bind in str_binds {
        let mut env = Env::with_def(st.def(str_dec.into()));
        st.push_prefix(str_bind.name.clone());
        get_str_exp(st, bs, ars, &mut env, str_bind.str_exp);
        st.pop_prefix();
        if let Some(e) = ins_no_dupe(&mut str_env, str_bind.name.clone(), env, Item::Struct) {
          st.err(str_dec, e);
        }
      }
      ac.as_mut_env().str_env.extend(str_env);
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
        let marker = st.syms.mark();
        let mut env = Env::with_def(st.def(str_dec.into()));
        let should_push = match st.info.mode() {
          Mode::Regular(_, _) => true,
          // `datatype option` is well-known
          Mode::BuiltinLib(_) => sig_bind.name.as_str() != "OPTION",
          Mode::PathOrder => false,
        };
        if should_push {
          st.push_prefix(sig_bind.name.clone());
        }
        get_sig_exp(st, bs, ars, &mut env, sig_bind.sig_exp);
        if should_push {
          st.pop_prefix();
        }
        let sig = env_to_sig(env, marker);
        if let Some(e) = ins_no_dupe(&mut sig_env, sig_bind.name.clone(), sig, Item::Sig) {
          st.err(str_dec, e);
        }
      }
      ac.as_mut_sig_env().extend(sig_env);
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
        let marker = st.syms.mark();
        get_sig_exp(st, bs, ars, &mut param_env, fun_bind.param_sig);
        let param_sig = env_to_sig(param_env, marker);
        let mut bs_clone = bs.clone();
        bs_clone.env.push(Env {
          str_env: map([(fun_bind.param_name.clone(), param_sig.env.clone())]),
          ..Default::default()
        });
        let mut body_env = Env::with_def(st.def(str_dec.into()));
        let marker = st.syms.mark();
        st.push_prefix(str_util::Name::new(&format!("{}(...)", fun_bind.functor_name)));
        get_str_exp(st, &bs_clone, ars, &mut body_env, fun_bind.body);
        st.pop_prefix();
        let mut body_ty_names = TyNameSet::default();
        env_syms::get(&body_env, &mut |x| {
          if x.generated_after(marker) {
            body_ty_names.insert(x);
          }
        });
        for sym in &param_sig.ty_names {
          body_ty_names.remove(sym);
        }
        let fun_name = fun_bind.functor_name.clone();
        let fun_sig = FunSig { param: param_sig, body_ty_names, body_env, flavor: fun_bind.flavor };
        if let Some(e) = ins_no_dupe(&mut fun_env, fun_name, fun_sig, Item::Functor) {
          st.err(str_dec, e);
        }
      }
      ac.as_mut_fun_env().extend(fun_env);
    }
    // @def(58)
    sml_hir::StrDec::Local(local_dec, in_dec) => {
      let mut local_bs = Bs::default();
      get_str_dec(st, bs, ars, StrDecAc::Bs(&mut local_bs), *local_dec);
      let mut bs = bs.clone();
      bs.append(local_bs);
      get_str_dec(st, &bs, ars, ac, *in_dec);
    }
    // @def(59), @def(60)
    sml_hir::StrDec::Seq(str_decs) => {
      let mut bs = bs.clone();
      match ac {
        StrDecAc::Env(ac) => {
          let mut one_env = Env::default();
          for &str_dec in str_decs {
            get_str_dec(st, &bs, ars, StrDecAc::Env(&mut one_env), str_dec);
            bs.env.push(one_env.clone());
            ac.append(&mut one_env);
          }
        }
        StrDecAc::Bs(ac) => {
          for &str_dec in str_decs {
            let mut one_bs = Bs::default();
            get_str_dec(st, &bs, ars, StrDecAc::Bs(&mut one_bs), str_dec);
            bs.append(one_bs.clone());
            ac.append(one_bs);
          }
        }
      }
    }
  }
}

fn get_str_exp(
  st: &mut St,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  ac: &mut Env,
  str_exp: sml_hir::StrExpIdx,
) {
  let str_exp = match str_exp {
    Some(x) => x,
    None => return,
  };
  match &ars.str_exp[str_exp] {
    // @def(50)
    sml_hir::StrExp::Struct(str_dec) => get_str_dec(st, bs, ars, StrDecAc::Env(ac), *str_dec),
    // @def(51)
    sml_hir::StrExp::Path(path) => match get_env_from_str_path(&bs.env, path) {
      Ok(got_env) => {
        st.info.insert(str_exp.into(), None, got_env.def);
        ac.append(&mut got_env.clone());
      }
      Err(e) => st.err(str_exp, e),
    },
    // @def(52), @def(53)
    sml_hir::StrExp::Ascription(inner_str_exp, asc, sig_exp) => {
      let mut str_exp_env = Env::default();
      get_str_exp(st, bs, ars, &mut str_exp_env, *inner_str_exp);
      let mut sig_exp_env = Env::default();
      let marker = st.syms.mark();
      let ov = get_sig_exp(st, bs, ars, &mut sig_exp_env, *sig_exp);
      let sig = env_to_sig(sig_exp_env, marker);
      let mut subst = realize::TyRealization::default();
      let mut to_add = sig.env.clone();
      let idx = sml_hir::Idx::from(str_exp);
      match st.info.mode() {
        Mode::Regular(_, _) => {
          env_instance_sig(st, idx, &mut subst, &str_exp_env, &sig);
          realize::get_env(st, idx, &subst, &mut to_add);
          enrich::get_env(st, idx, &str_exp_env, &to_add);
        }
        Mode::BuiltinLib(_) | Mode::PathOrder => {}
      }
      if matches!(asc, sml_hir::Ascription::Opaque) {
        subst.clear();
        gen_fresh_syms(st, idx, &mut subst, &sig.ty_names);
        to_add = sig.env.clone();
        realize::get_env(st, idx, &subst, &mut to_add);
      }
      if let Some(ov) = ov {
        let ty_info = to_add.ty_env.get(ov.as_str()).expect("no overloaded ty");
        match &ty_info.ty_scheme.ty {
          Ty::Con(arguments, sym) => {
            assert!(arguments.is_empty());
            st.syms.overloads_mut()[ov].push(*sym);
          }
          t => unreachable!("overload not a Con: {t:?}"),
        }
      }
      ac.append(&mut to_add);
    }
    // @def(54)
    sml_hir::StrExp::App(fun_name, arg_str_exp, flavor) => match bs.fun_env.get(fun_name) {
      Some(fun_sig) => {
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
        env_instance_sig(st, arg_idx, &mut subst, &arg_env, &fun_sig.param);
        gen_fresh_syms(st, idx, &mut subst, &fun_sig.body_ty_names);
        realize::get_env(st, idx, &subst, &mut to_add);
        let mut param_env = fun_sig.param.env.clone();
        realize::get_env(st, arg_idx, &subst, &mut param_env);
        enrich::get_env(st, arg_idx, &arg_env, &param_env);
        let def = st.def(idx);
        for env in to_add.str_env.values_mut() {
          env.def = def;
        }
        for ty_env in to_add.ty_env.values_mut() {
          ty_env.def = def;
        }
        for val_info in to_add.val_env.values_mut() {
          val_info.def = def;
        }
        st.info.insert(idx, None, fun_sig.body_env.def);
        ac.append(&mut to_add);
      }
      None => st.err(str_exp, ErrorKind::Undefined(Item::Functor, fun_name.clone())),
    },
    // @def(55)
    sml_hir::StrExp::Let(str_dec, str_exp) => {
      let mut let_env = Env::default();
      get_str_dec(st, bs, ars, StrDecAc::Env(&mut let_env), *str_dec);
      let mut bs = bs.clone();
      bs.env.append(&mut let_env);
      get_str_exp(st, &bs, ars, ac, *str_exp);
    }
  }
}

fn get_sig_exp(
  st: &mut St,
  bs: &Bs,
  ars: &sml_hir::Arenas,
  ac: &mut Env,
  sig_exp: sml_hir::SigExpIdx,
) -> Option<BasicOverload> {
  let sig_exp = match sig_exp {
    Some(x) => x,
    None => return None,
  };
  match &ars.sig_exp[sig_exp] {
    // @def(62)
    sml_hir::SigExp::Spec(spec) => {
      get_spec(st, bs, ars, ac, *spec);
      None
    }
    // @def(63)
    sml_hir::SigExp::Name(name) => match bs.sig_env.get(name) {
      Some(sig) => {
        let mut subst = realize::TyRealization::default();
        let idx = sml_hir::Idx::from(sig_exp);
        gen_fresh_syms(st, idx, &mut subst, &sig.ty_names);
        let mut sig_env = sig.env.clone();
        realize::get_env(st, idx, &subst, &mut sig_env);
        st.info.insert(idx, None, sig.env.def);
        ac.append(&mut sig_env);
        match st.info.mode() {
          Mode::BuiltinLib(_) => match name.as_str() {
            "WORD" => Some(BasicOverload::Word),
            "INTEGER" | "INT_INF" => Some(BasicOverload::Int),
            "REAL" => Some(BasicOverload::Real),
            _ => None,
          },
          Mode::Regular(_, _) | Mode::PathOrder => None,
        }
      }
      None => {
        st.err(sig_exp, ErrorKind::Undefined(Item::Sig, name.clone()));
        None
      }
    },
    // @def(64)
    sml_hir::SigExp::Where(inner, kind) => {
      let marker = st.syms.mark();
      let mut inner_env = Env::default();
      let ov = get_sig_exp(st, bs, ars, &mut inner_env, *inner);
      get_where_kind(st, sig_exp.into(), bs, marker, ars, &mut inner_env, kind);
      ac.append(&mut inner_env);
      ov
    }
  }
}

fn get_where_kind(
  st: &mut St,
  idx: sml_hir::Idx,
  bs: &Bs,
  marker: SymsMarker,
  ars: &sml_hir::Arenas,
  inner_env: &mut Env,
  kind: &sml_hir::WhereKind,
) {
  match kind {
    sml_hir::WhereKind::Type(ty_vars, path, ty) => {
      let mut cx = bs.as_cx();
      let fixed = dec::add_fixed_ty_vars(st, idx, &mut cx, TyVarSrc::Ty, ty_vars);
      let mut ty_scheme = TyScheme::zero(ty::get(st, &cx, ars, ty::Mode::TyRhs, *ty));
      generalize_fixed(fixed, &mut ty_scheme);
      match get_where_type(st, idx, marker, inner_env, path, ty_scheme) {
        Ok(()) => {}
        Err(e) => st.err(idx, e),
      }
    }
    sml_hir::WhereKind::Structure(lhs, rhs) => {
      let lhs_ty_cons = match get_path_ty_cons(inner_env, lhs) {
        Ok(x) => x,
        Err(e) => {
          st.err(idx, e);
          return;
        }
      };
      let rhs_ty_cons = match get_path_ty_cons(&bs.env, rhs) {
        Ok(x) => x,
        Err(e) => {
          st.err(idx, e);
          return;
        }
      };
      for ty_con in lhs_ty_cons {
        if !rhs_ty_cons.contains(&ty_con) {
          continue;
        }
        let lhs = join_paths(lhs, &ty_con);
        let rhs = join_paths(rhs, &ty_con);
        match get_ty_info(&bs.env, &rhs) {
          Ok(ty_info) => {
            let ty_scheme = ty_info.ty_scheme.clone();
            // HACK: intentionally ignore CannotRealizeTy. I'm not exactly sure of the semantics of
            // `where S = T` but this silences some errors seen in valid NJ-flavored SML.
            match get_where_type(st, idx, marker, inner_env, &lhs, ty_scheme) {
              Ok(()) | Err(ErrorKind::CannotRealizeTy(_, _)) => {}
              Err(e) => st.err(idx, e),
            }
          }
          Err(e) => st.err(idx, e),
        }
      }
    }
  }
}

fn get_where_type(
  st: &mut St,
  idx: sml_hir::Idx,
  marker: SymsMarker,
  inner_env: &mut Env,
  path: &sml_hir::Path,
  ty_scheme: TyScheme,
) -> Result<(), ErrorKind> {
  let ty_info = get_ty_info(inner_env, path)?;
  match &ty_info.ty_scheme.ty {
    Ty::None => Ok(()),
    // TODO side condition for well-formed?
    Ty::Con(_, sym) => {
      if sym.generated_after(marker) {
        realize::get_env(st, idx, &map([(*sym, ty_scheme)]), inner_env);
        Ok(())
      } else {
        // @test(sig::impossible)
        Err(ErrorKind::CannotRealizeTy(path.clone(), ty_info.ty_scheme.clone()))
      }
    }
    // @test(sig::where_not_con)
    _ => Err(ErrorKind::CannotRealizeTy(path.clone(), ty_info.ty_scheme.clone())),
  }
}

fn gen_fresh_syms(
  st: &mut St,
  idx: sml_hir::Idx,
  subst: &mut realize::TyRealization,
  ty_names: &TyNameSet,
) {
  let mut ac = Vec::<(StartedSym, TyInfo, Equality)>::new();
  for &sym in ty_names.iter() {
    let sym_info = st.syms.get(sym).unwrap();
    let mut ty_info = sym_info.ty_info.clone();
    let equality = sym_info.equality;
    let started = st.syms.start(sym_info.path.clone());
    let ty_scheme = TyScheme::n_ary(ty_info.ty_scheme.bound_vars.iter().cloned(), started.sym());
    ty_info.ty_scheme = ty_scheme.clone();
    ac.push((started, ty_info, equality));
    assert!(subst.insert(sym, ty_scheme).is_none());
  }
  for (started, mut ty_info, equality) in ac {
    realize::get_val_env(st, idx, subst, &mut ty_info.val_env);
    st.syms.finish(started, ty_info, equality);
  }
}

// @def(65)
fn env_to_sig(env: Env, marker: SymsMarker) -> Sig {
  let mut ty_names = TyNameSet::default();
  env_syms::get(&env, &mut |x| {
    if x.generated_after(marker) {
      ty_names.insert(x);
    }
  });
  Sig { ty_names, env }
}

fn get_spec(st: &mut St, bs: &Bs, ars: &sml_hir::Arenas, ac: &mut Env, spec: sml_hir::SpecIdx) {
  let spec = match spec {
    Some(x) => x,
    None => return,
  };
  match &ars.spec[spec] {
    // @def(68)
    sml_hir::Spec::Val(ty_vars, val_descs) => {
      // @def(79)
      let mut cx = bs.as_cx();
      let fixed = dec::add_fixed_ty_vars(st, spec.into(), &mut cx, TyVarSrc::Val, ty_vars);
      for val_desc in val_descs {
        let mut ty_scheme = TyScheme::zero(ty::get(st, &cx, ars, ty::Mode::Regular, val_desc.ty));
        let mv_g = st.meta_gen.generalizer();
        generalize(mv_g, &st.subst, fixed.clone(), &mut ty_scheme)
          .expect("a type cannot have record meta vars because it has no patterns");
        let vi = ValInfo { ty_scheme, id_status: IdStatus::Val, def: st.def(spec.into()) };
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
    sml_hir::Spec::Datatype(dat_desc) => {
      let dat_descs = std::slice::from_ref(dat_desc);
      let (ty_env, big_val_env) =
        dec::get_dat_binds(st, spec.into(), bs.as_cx(), ars, dat_descs, &[]);
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
    sml_hir::Spec::DatatypeCopy(name, path) => match get_ty_info(&bs.env, path) {
      Ok(ty_info) => {
        let ins = ins_no_dupe(&mut ac.ty_env, name.clone(), ty_info.clone(), Item::Ty);
        if let Some(e) = ins {
          st.err(spec, e);
        }
        for (name, val_info) in &ty_info.val_env {
          let ins = ins_no_dupe(&mut ac.val_env, name.clone(), val_info.clone(), Item::Val);
          if let Some(e) = ins {
            st.err(spec, e);
          }
        }
      }
      Err(e) => st.err(spec, e),
    },
    // @def(73), @def(83)
    sml_hir::Spec::Exception(ex_desc) => {
      let cx = bs.as_cx();
      // almost the same as the logic in dec::get, save for the check for ty vars.
      let mut ty = Ty::EXN;
      let param = ex_desc.ty.map(|param| ty::get(st, &cx, ars, ty::Mode::Regular, param));
      if let Some(ref param) = param {
        ty = Ty::fun(param.clone(), ty);
      }
      let exn = st.syms.insert_exn(st.mk_path(ex_desc.name.clone()), param);
      let vi = ValInfo {
        ty_scheme: TyScheme::zero(ty),
        id_status: IdStatus::Exn(exn),
        def: st.def(spec.into()),
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
      let marker = st.syms.mark();
      let mut inner_env = Env::default();
      get_spec(st, bs, ars, &mut inner_env, *inner);
      match kind {
        sml_hir::SharingKind::Regular => {
          sharing_ty::get(st, spec.into(), marker, &mut inner_env, paths);
        }
        sml_hir::SharingKind::Derived => {
          let mut all: Vec<_> = paths
            .iter()
            .filter_map(|path| match get_path_ty_cons(&inner_env, path) {
              Ok(ty_cons) => Some((path, ty_cons)),
              Err(e) => {
                st.err(spec, e);
                None
              }
            })
            .collect();
          while let Some((struct_1, ty_cons_1)) = all.pop() {
            for ty_con in ty_cons_1 {
              for (struct_2, ty_cons_2) in &all {
                if !ty_cons_2.contains(&ty_con) {
                  continue;
                }
                let path_1 = join_paths(struct_1, &ty_con);
                let path_2 = join_paths(struct_2, &ty_con);
                sharing_ty::get(st, spec.into(), marker, &mut inner_env, &[path_1, path_2]);
              }
            }
          }
        }
      }
      append_no_dupe(st, spec.into(), ac, &mut inner_env);
    }
    // @def(76), @def(77)
    sml_hir::Spec::Seq(specs) => {
      let mut bs = bs.clone();
      let mut one_env = Env::default();
      for &seq_spec in specs {
        get_spec(st, &bs, ars, &mut one_env, seq_spec);
        bs.env.append(&mut one_env.clone());
        append_no_dupe(st, seq_spec.unwrap_or(spec).into(), ac, &mut one_env);
      }
    }
  }
}

/// empties other into ac, while checking for dupes.
fn append_no_dupe(st: &mut St, idx: sml_hir::Idx, ac: &mut Env, other: &mut Env) {
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

fn get_path_ty_cons<E: EnvLike>(
  env: &E,
  path: &sml_hir::Path,
) -> Result<FxHashSet<sml_hir::Path>, ErrorKind> {
  let got_env = get_env_from_str_path(env, path)?;
  let mut ty_cons = FxHashSet::<sml_hir::Path>::default();
  get_ty_cons(&mut Vec::new(), &mut ty_cons, got_env);
  Ok(ty_cons)
}

fn join_paths(p1: &sml_hir::Path, p2: &sml_hir::Path) -> sml_hir::Path {
  sml_hir::Path::new(p1.all_names().chain(p2.prefix()).cloned(), p2.last().clone())
}

fn get_ty_cons(prefix: &mut Vec<str_util::Name>, ac: &mut FxHashSet<sml_hir::Path>, env: &Env) {
  ac.extend(env.ty_env.keys().map(|name| sml_hir::Path::new(prefix.clone(), name.clone())));
  for (name, env) in &env.str_env {
    prefix.push(name.clone());
    get_ty_cons(prefix, ac, env);
    prefix.pop().unwrap();
  }
}

// @def(80)
fn get_ty_desc(
  st: &mut St,
  idx: sml_hir::Idx,
  ty_env: &mut TyEnv,
  ty_desc: &sml_hir::TyDesc,
  equality: Equality,
) {
  let mut ty_vars = FxHashSet::<&sml_hir::TyVar>::default();
  let started = st.syms.start(st.mk_path(ty_desc.name.clone()));
  for ty_var in &ty_desc.ty_vars {
    if !ty_vars.insert(ty_var) {
      let e = ErrorKind::Duplicate(Item::TyVar, ty_var.as_name().clone());
      st.err(idx, e);
    }
  }
  let ty_info = TyInfo {
    ty_scheme: TyScheme::n_ary(
      ty_desc.ty_vars.iter().map(|x| x.is_equality().then_some(TyVarKind::Equality)),
      started.sym(),
    ),
    val_env: ValEnv::default(),
    def: st.def(idx),
  };
  st.syms.finish(started, ty_info.clone(), equality);
  if let Some(e) = ins_no_dupe(ty_env, ty_desc.name.clone(), ty_info, Item::Ty) {
    st.err(idx, e);
  }
}

fn env_instance_sig(
  st: &mut St,
  idx: sml_hir::Idx,
  subst: &mut realize::TyRealization,
  env: &Env,
  sig: &Sig,
) {
  for &sym in &sig.ty_names {
    let mut path = Vec::<&str_util::Name>::new();
    let ty_scheme =
      TyScheme::n_ary(st.syms.get(sym).unwrap().ty_info.ty_scheme.bound_vars.iter().cloned(), sym);
    if !bound_ty_name_to_path(st, &mut path, &sig.env, &ty_scheme) {
      // @test(sig::no_path_to_sym). there should have already been an error emitted for this
      log::warn!("no path to sym");
      return;
    }
    let last = path.pop().unwrap();
    match get_ty_info_raw(env, path, last) {
      Ok(ty_info) => {
        subst.insert(sym, ty_info.ty_scheme.clone());
      }
      Err(e) => st.err(idx, e),
    }
  }
}

/// note that given an environment for the signature:
///
/// ```sml
/// signature SIG = sig
///   type t
///   type u = t
/// end
/// ```
///
/// and a request to find the path to the single ty name bound by this signature's env (there is
/// only one), this function will report _either_ `t` or `u` based on which one comes up first in
/// the `iter()` order.
///
/// this seems slightly questionable, but I'm not actually sure if it's an issue. I mean, it says
/// right there that they should be equal anyway.
fn bound_ty_name_to_path<'e>(
  st: &mut St,
  ac: &mut Vec<&'e str_util::Name>,
  env: &'e Env,
  ty_scheme: &TyScheme,
) -> bool {
  for (name, ty_info) in &env.ty_env {
    if eq_ty_fn_no_emit(st, ty_info.ty_scheme.clone(), ty_scheme.clone()).is_ok() {
      ac.push(name);
      return true;
    }
  }
  for (name, env) in &env.str_env {
    ac.push(name);
    if bound_ty_name_to_path(st, ac, env, ty_scheme) {
      return true;
    }
    ac.pop();
  }
  false
}
