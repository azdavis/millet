use crate::compatible::{eq_ty_fn, eq_ty_fn_no_emit, generalizes};
use crate::config::Cfg;
use crate::error::{ErrorKind, Item};
use crate::get_env::{get_env_from_str_path, get_ty_info, get_ty_info_raw};
use crate::info::Mode;
use crate::st::St;
use crate::types::{
  generalize, generalize_fixed, BasicOverload, Bs, Env, EnvLike, EnvStack, FunEnv, FunSig,
  HasRecordMetaVars, IdStatus, Sig, SigEnv, StartedSym, StrEnv, Sym, Ty, TyEnv, TyInfo, TyNameSet,
  TyScheme, TyVarKind, TyVarSrc, ValEnv, ValInfo,
};
use crate::util::{apply_bv, ignore, ins_check_name, ins_no_dupe, ty_syms};
use crate::{dec, ty};
use fast_hash::{map, FxHashMap, FxHashSet};

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
    // Def(56)
    sml_hir::StrDec::Dec(dec) => {
      dec::get(st, Cfg::default(), &bs.as_cx(), ars, ac.as_mut_env(), *dec);
    }
    // Def(57)
    sml_hir::StrDec::Structure(str_binds) => {
      // Def(61)
      let mut str_env = StrEnv::default();
      for str_bind in str_binds {
        let mut env = Env::with_def(st.def(str_dec.into()));
        get_str_exp(st, bs, ars, &mut env, str_bind.str_exp);
        if let Some(e) = ins_no_dupe(&mut str_env, str_bind.name.clone(), env, Item::Struct) {
          st.err(str_dec, e);
        }
      }
      ac.as_mut_env().str_env.extend(str_env);
    }
    // Def(58)
    sml_hir::StrDec::Local(local_dec, in_dec) => {
      let mut local_bs = Bs::default();
      get_str_dec(st, bs, ars, StrDecAc::Bs(&mut local_bs), *local_dec);
      let mut bs = bs.clone();
      bs.append(local_bs);
      get_str_dec(st, &bs, ars, ac, *in_dec);
    }
    // Def(59), Def(60)
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
    // Def(66), Def(88)
    sml_hir::StrDec::Signature(sig_binds) => {
      let ac = match ac {
        StrDecAc::Bs(ac) => ac,
        StrDecAc::Env(_) => {
          st.err(str_dec, ErrorKind::DecNotAllowedHere);
          return;
        }
      };
      let mut sig_env = SigEnv::default();
      // Def(67)
      for sig_bind in sig_binds {
        let mut env = Env::with_def(st.def(str_dec.into()));
        get_sig_exp(st, bs, ars, &mut env, sig_bind.sig_exp);
        let sig = env_to_sig(bs, env);
        if let Some(e) = ins_no_dupe(&mut sig_env, sig_bind.name.clone(), sig, Item::Sig) {
          st.err(str_dec, e);
        }
      }
      ac.as_mut_sig_env().extend(sig_env);
    }
    // Def(85), Def(89)
    sml_hir::StrDec::Functor(fun_binds) => {
      let ac = match ac {
        StrDecAc::Bs(ac) => ac,
        StrDecAc::Env(_) => {
          st.err(str_dec, ErrorKind::DecNotAllowedHere);
          return;
        }
      };
      let mut fun_env = FunEnv::default();
      // Def(86)
      for fun_bind in fun_binds {
        let mut param_env = Env::default();
        get_sig_exp(st, bs, ars, &mut param_env, fun_bind.param_sig);
        let param_sig = env_to_sig(bs, param_env);
        let mut bs_clone = bs.clone();
        bs_clone.env.push(Env {
          str_env: map([(fun_bind.param_name.clone(), param_sig.env.clone())]),
          ..Default::default()
        });
        let mut body_env = Env::with_def(st.def(str_dec.into()));
        get_str_exp(st, &bs_clone, ars, &mut body_env, fun_bind.body);
        let mut body_ty_names = TyNameSet::default();
        env_syms(&mut |x| ignore(body_ty_names.insert(x)), &body_env);
        bs_syms(&mut |x| ignore(body_ty_names.remove(&x)), &bs_clone);
        for sym in &param_sig.ty_names {
          body_ty_names.remove(sym);
        }
        let fun_name = fun_bind.functor_name.clone();
        let fun_sig = FunSig { param: param_sig, body_ty_names, body_env };
        if let Some(e) = ins_no_dupe(&mut fun_env, fun_name, fun_sig, Item::Functor) {
          st.err(str_dec, e);
        }
      }
      ac.as_mut_fun_env().extend(fun_env);
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
    // Def(50)
    sml_hir::StrExp::Struct(str_dec) => get_str_dec(st, bs, ars, StrDecAc::Env(ac), *str_dec),
    // Def(51)
    sml_hir::StrExp::Path(path) => match get_env_from_str_path(&bs.env, path) {
      Ok(got_env) => {
        st.info().insert(str_exp.into(), None, got_env.def);
        ac.append(&mut got_env.clone());
      }
      Err(e) => st.err(str_exp, e),
    },
    // Def(52), Def(53)
    sml_hir::StrExp::Ascription(inner_str_exp, asc, sig_exp) => {
      let mut str_exp_env = Env::default();
      get_str_exp(st, bs, ars, &mut str_exp_env, *inner_str_exp);
      let mut sig_exp_env = Env::default();
      let ov = get_sig_exp(st, bs, ars, &mut sig_exp_env, *sig_exp);
      let sig = env_to_sig(bs, sig_exp_env);
      let mut subst = TyRealization::default();
      let mut to_add = sig.env.clone();
      match st.mode() {
        Mode::Regular(_) => {
          env_instance_sig(st, &mut subst, &str_exp_env, &sig, str_exp.into());
          env_realize(&subst, &mut to_add);
          env_enrich(st, &str_exp_env, &to_add, str_exp.into());
        }
        Mode::StdBasis(_) => {}
      }
      if matches!(asc, sml_hir::Ascription::Opaque) {
        subst.clear();
        gen_fresh_syms(st, &mut subst, &sig.ty_names);
        to_add = sig.env.clone();
        env_realize(&subst, &mut to_add);
      }
      if let Some(ov) = ov {
        let ty_info = to_add.ty_env.get(ov.as_str()).expect("no overloaded ty");
        match &ty_info.ty_scheme.ty {
          Ty::Con(arguments, sym) => {
            assert!(arguments.is_empty());
            st.syms.overloads()[ov].push(*sym);
          }
          t => unreachable!("overload not a Con: {t:?}"),
        }
      }
      ac.append(&mut to_add);
    }
    // Def(54)
    sml_hir::StrExp::App(fun_name, arg_str_exp) => match bs.fun_env.get(fun_name) {
      Some(fun_sig) => {
        let mut arg_env = Env::default();
        get_str_exp(st, bs, ars, &mut arg_env, *arg_str_exp);
        let mut subst = TyRealization::default();
        let mut to_add = fun_sig.body_env.clone();
        let arg_idx = sml_hir::Idx::from(arg_str_exp.unwrap_or(str_exp));
        env_instance_sig(st, &mut subst, &arg_env, &fun_sig.param, arg_idx);
        gen_fresh_syms(st, &mut subst, &fun_sig.body_ty_names);
        env_realize(&subst, &mut to_add);
        let mut param_env = fun_sig.param.env.clone();
        env_realize(&subst, &mut param_env);
        env_enrich(st, &arg_env, &param_env, arg_idx);
        let def = st.def(str_exp.into());
        for env in to_add.str_env.values_mut() {
          env.def = def;
        }
        for ty_env in to_add.ty_env.values_mut() {
          ty_env.def = def;
        }
        for val_info in to_add.val_env.values_mut() {
          val_info.def = def;
        }
        st.info().insert(str_exp.into(), None, fun_sig.body_env.def);
        ac.append(&mut to_add);
      }
      None => st.err(str_exp, ErrorKind::Undefined(Item::Functor, fun_name.clone())),
    },
    // Def(55)
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
    // Def(62)
    sml_hir::SigExp::Spec(spec) => {
      get_spec(st, bs, ars, ac, *spec);
      None
    }
    // Def(63)
    sml_hir::SigExp::Name(name) => match bs.sig_env.get(name) {
      Some(sig) => {
        let mut subst = TyRealization::default();
        gen_fresh_syms(st, &mut subst, &sig.ty_names);
        let mut sig_env = sig.env.clone();
        env_realize(&subst, &mut sig_env);
        st.info().insert(sig_exp.into(), None, sig.env.def);
        ac.append(&mut sig_env);
        match st.mode() {
          Mode::StdBasis(_) => match name.as_str() {
            "WORD" => Some(BasicOverload::Word),
            "INTEGER" | "INT_INF" => Some(BasicOverload::Int),
            "REAL" => Some(BasicOverload::Real),
            _ => None,
          },
          Mode::Regular(_) => None,
        }
      }
      None => {
        st.err(sig_exp, ErrorKind::Undefined(Item::Sig, name.clone()));
        None
      }
    },
    // Def(64)
    sml_hir::SigExp::WhereType(inner, ty_vars, path, ty) => {
      let mut inner_env = Env::default();
      let ov = get_sig_exp(st, bs, ars, &mut inner_env, *inner);
      let mut cx = bs.as_cx();
      let fixed = dec::add_fixed_ty_vars(st, &mut cx, TyVarSrc::Ty, ty_vars, sig_exp.into());
      let mut ty_scheme = TyScheme::zero(ty::get(st, &cx, ars, ty::Mode::TyRhs, *ty));
      generalize_fixed(fixed, &mut ty_scheme);
      get_where_type(st, &mut inner_env, path, ty_scheme, sig_exp.into());
      ac.append(&mut inner_env);
      ov
    }
    // SML/NJ extension
    sml_hir::SigExp::Where(inner, lhs, rhs) => {
      let mut inner_env = Env::default();
      let ov = get_sig_exp(st, bs, ars, &mut inner_env, *inner);
      let lhs_ty_cons = match get_path_ty_cons(&inner_env, lhs) {
        Ok(x) => x,
        Err(e) => {
          st.err(sig_exp, e);
          return ov;
        }
      };
      let rhs_ty_cons = match get_path_ty_cons(&bs.env, rhs) {
        Ok(x) => x,
        Err(e) => {
          st.err(sig_exp, e);
          return ov;
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
            get_where_type(st, &mut inner_env, &lhs, ty_scheme, sig_exp.into());
          }
          Err(e) => st.err(sig_exp, e),
        }
      }
      ac.append(&mut inner_env);
      ov
    }
  }
}

fn get_where_type(
  st: &mut St,
  inner_env: &mut Env,
  path: &sml_hir::Path,
  ty_scheme: TyScheme,
  idx: sml_hir::Idx,
) {
  let got_len = ty_scheme.bound_vars.len();
  match get_ty_info(inner_env, path) {
    Ok(ty_info) => {
      let want_len = ty_info.ty_scheme.bound_vars.len();
      if want_len == got_len {
        match &ty_info.ty_scheme.ty {
          Ty::None => {}
          // TODO side condition for sym not in T of B?
          // TODO side condition for well-formed?
          Ty::Con(_, sym) => env_realize(&map([(*sym, ty_scheme)]), inner_env),
          t => unreachable!("bad `where`: {t:?}"),
        }
      } else {
        st.err(idx, ErrorKind::WrongNumTyArgs(want_len, got_len));
      }
    }
    Err(e) => st.err(idx, e),
  }
}

fn gen_fresh_syms(st: &mut St, subst: &mut TyRealization, ty_names: &TyNameSet) {
  let mut ac = Vec::<(StartedSym, TyInfo)>::new();
  for &sym in ty_names.iter() {
    let (name, ty_info) = st.syms.get(sym).unwrap();
    let name = name.clone();
    let mut ty_info = ty_info.clone();
    let started = st.syms.start(name);
    let ty_scheme = TyScheme::n_ary(ty_info.ty_scheme.bound_vars.kinds().cloned(), started.sym());
    ty_info.ty_scheme = ty_scheme.clone();
    ac.push((started, ty_info));
    assert!(subst.insert(sym, ty_scheme).is_none());
  }
  for (started, mut ty_info) in ac {
    val_env_realize(subst, &mut ty_info.val_env);
    st.syms.finish(started, ty_info);
  }
}

// Def(65)
fn env_to_sig(bs: &Bs, env: Env) -> Sig {
  let mut ty_names = TyNameSet::default();
  env_syms(&mut |x| ignore(ty_names.insert(x)), &env);
  bs_syms(&mut |x| ignore(ty_names.remove(&x)), bs);
  Sig { ty_names, env }
}

fn get_spec(st: &mut St, bs: &Bs, ars: &sml_hir::Arenas, ac: &mut Env, spec: sml_hir::SpecIdx) {
  let spec = match spec {
    Some(x) => x,
    None => return,
  };
  match &ars.spec[spec] {
    // Def(68)
    sml_hir::Spec::Val(ty_vars, val_descs) => {
      // Def(79)
      let mut cx = bs.as_cx();
      let fixed = dec::add_fixed_ty_vars(st, &mut cx, TyVarSrc::Val, ty_vars, spec.into());
      for val_desc in val_descs {
        let mut ty_scheme = TyScheme::zero(ty::get(st, &cx, ars, ty::Mode::Regular, val_desc.ty));
        let mv_g = st.meta_gen.generalizer();
        let g = generalize(mv_g, st.subst(), fixed.clone(), &mut ty_scheme);
        assert_ty_has_no_record_meta_vars(g);
        let vi = ValInfo { ty_scheme, id_status: IdStatus::Val, def: st.def(spec.into()) };
        let name = &val_desc.name;
        if let Some(e) = ins_check_name(&mut ac.val_env, name.clone(), vi, Item::Val) {
          st.err(spec, e);
        }
      }
    }
    // Def(69)
    //
    // Def(70)
    //
    // TODO equality checks
    sml_hir::Spec::Ty(ty_descs) | sml_hir::Spec::EqTy(ty_descs) => {
      get_ty_desc(st, &mut ac.ty_env, ty_descs, spec.into());
    }
    // Def(71)
    sml_hir::Spec::Datatype(dat_desc) => {
      let dat_descs = std::slice::from_ref(dat_desc);
      let (ty_env, big_val_env) =
        dec::get_dat_binds(st, bs.as_cx(), ars, dat_descs, &[], spec.into());
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
    // Def(72)
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
    // Def(73), Def(83)
    sml_hir::Spec::Exception(ex_desc) => {
      let cx = bs.as_cx();
      // almost the same as the logic in dec::get, save for the check for ty vars.
      let mut ty = Ty::EXN;
      let param = ex_desc.ty.map(|param| ty::get(st, &cx, ars, ty::Mode::Regular, param));
      if let Some(ref param) = param {
        ty = Ty::fun(param.clone(), ty);
      }
      let exn = st.syms.insert_exn(ex_desc.name.clone(), param);
      let vi = ValInfo {
        ty_scheme: TyScheme::zero(ty),
        id_status: IdStatus::Exn(exn),
        def: st.def(spec.into()),
      };
      if let Some(e) = ins_check_name(&mut ac.val_env, ex_desc.name.clone(), vi, Item::Val) {
        st.err(spec, e);
      }
    }
    // Def(74), Def(84)
    sml_hir::Spec::Str(str_desc) => {
      let mut one_env = Env::default();
      get_sig_exp(st, bs, ars, &mut one_env, str_desc.sig_exp);
      let name = str_desc.name.clone();
      if let Some(e) = ins_no_dupe(&mut ac.str_env, name, one_env, Item::Struct) {
        st.err(spec, e);
      }
    }
    // Def(75)
    sml_hir::Spec::Include(sig_exp) => {
      get_sig_exp(st, bs, ars, ac, *sig_exp);
    }
    // Def(78)
    sml_hir::Spec::Sharing(inner, kind, paths) => {
      let mut inner_env = Env::default();
      get_spec(st, bs, ars, &mut inner_env, *inner);
      match kind {
        sml_hir::SharingKind::Regular => get_sharing_type(st, &mut inner_env, paths, spec.into()),
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
                let path_2 = join_paths(*struct_2, &ty_con);
                get_sharing_type(st, &mut inner_env, &[path_1, path_2], spec.into());
              }
            }
          }
        }
      }
      append_no_dupe(st, ac, &mut inner_env, spec.into());
    }
    // Def(76), Def(77)
    sml_hir::Spec::Seq(specs) => {
      let mut bs = bs.clone();
      let mut one_env = Env::default();
      for &seq_spec in specs {
        get_spec(st, &bs, ars, &mut one_env, seq_spec);
        bs.env.append(&mut one_env.clone());
        append_no_dupe(st, ac, &mut one_env, seq_spec.unwrap_or(spec).into());
      }
    }
  }
}

/// empties other into ac, while checking for dupes.
fn append_no_dupe(st: &mut St, ac: &mut Env, other: &mut Env, idx: sml_hir::Idx) {
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

/// `sharing type` directly uses this, and the `sharing` derived form eventually uses this.
fn get_sharing_type(st: &mut St, inner_env: &mut Env, paths: &[sml_hir::Path], idx: sml_hir::Idx) {
  let mut ty_scheme = None::<TyScheme>;
  let mut syms = Vec::<Sym>::with_capacity(paths.len());
  for path in paths {
    match get_ty_info(inner_env, path) {
      Ok(ty_info) => {
        // TODO assert exists c s.t. for all ty schemes, arity of ty scheme = c? and all other
        // things about the bound ty vars are 'compatible'? (the bit about admitting equality?)
        if let Ty::Con(_, sym) = &ty_info.ty_scheme.ty {
          if ty_scheme.is_none() {
            ty_scheme = Some(ty_info.ty_scheme.clone());
          }
          syms.push(*sym);
        }
        // TODO the else is reachable, but doesn't make a lot of sense.
      }
      Err(e) => st.err(idx, e),
    }
  }
  if let Some(ty_scheme) = ty_scheme {
    let subst: TyRealization = syms.into_iter().map(|sym| (sym, ty_scheme.clone())).collect();
    env_realize(&subst, inner_env);
  }
  // TODO the None case is reachable (because of the above), but bad.
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
  sml_hir::Path::new(p1.all_names().chain(p2.structures()).cloned(), p2.last().clone())
}

fn get_ty_cons(structures: &mut Vec<str_util::Name>, ac: &mut FxHashSet<sml_hir::Path>, env: &Env) {
  ac.extend(env.ty_env.keys().map(|name| sml_hir::Path::new(structures.clone(), name.clone())));
  for (name, env) in &env.str_env {
    structures.push(name.clone());
    get_ty_cons(structures, ac, env);
    structures.pop().unwrap();
  }
}

// Def(80). TODO equality checks
fn get_ty_desc(st: &mut St, ty_env: &mut TyEnv, ty_desc: &sml_hir::TyDesc, idx: sml_hir::Idx) {
  let mut ty_vars = FxHashSet::<&sml_hir::TyVar>::default();
  let started = st.syms.start(ty_desc.name.clone());
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
  st.syms.finish(started, ty_info.clone());
  if let Some(e) = ins_no_dupe(ty_env, ty_desc.name.clone(), ty_info, Item::Ty) {
    st.err(idx, e);
  }
}

type TyRealization = FxHashMap<Sym, TyScheme>;

fn env_instance_sig(
  st: &mut St,
  subst: &mut TyRealization,
  env: &Env,
  sig: &Sig,
  idx: sml_hir::Idx,
) {
  for &sym in &sig.ty_names {
    let mut path = Vec::<&str_util::Name>::new();
    let (_, ty_info) = st.syms.get(sym).unwrap();
    let ty_scheme = TyScheme::n_ary(ty_info.ty_scheme.bound_vars.kinds().cloned(), sym);
    if !bound_ty_name_to_path(st, &mut path, &sig.env, &ty_scheme) {
      unreachable!("couldn't get a path for {sym:?} in {sig:?}");
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

// TODO improve error messages/range for the enrich family of fns.
//
// for ranges, we might need to track `sml_hir::Idx`es either in the Env or in a separate map with
// exactly the same keys (names). or we could add a special env only for use here that has the
// indices?

fn env_enrich(st: &mut St, general: &Env, specific: &Env, idx: sml_hir::Idx) {
  for (name, specific) in &specific.str_env {
    match general.str_env.get(name) {
      Some(general) => env_enrich(st, general, specific, idx),
      None => st.err(idx, ErrorKind::Missing(Item::Struct, name.clone())),
    }
  }
  for (name, specific) in &specific.ty_env {
    match general.ty_env.get(name) {
      Some(general) => ty_info_enrich(st, general.clone(), specific.clone(), idx),
      None => st.err(idx, ErrorKind::Missing(Item::Ty, name.clone())),
    }
  }
  for (name, specific) in &specific.val_env {
    match general.val_env.get(name) {
      Some(general) => val_info_enrich(st, general.clone(), specific, name, idx),
      None => st.err(idx, ErrorKind::Missing(Item::Val, name.clone())),
    }
  }
}

fn ty_info_enrich(st: &mut St, mut general: TyInfo, specific: TyInfo, idx: sml_hir::Idx) {
  eq_ty_fn(st, specific.ty_scheme, general.ty_scheme.clone(), idx);
  if specific.val_env.is_empty() {
    return;
  }
  for (name, specific) in specific.val_env {
    match general.val_env.remove(&name) {
      Some(general) => {
        if !general.id_status.same_kind_as(&specific.id_status) {
          st.err(idx, ErrorKind::WrongIdStatus(name.clone()));
        }
        eq_ty_fn(st, specific.ty_scheme, general.ty_scheme.clone(), idx);
      }
      None => st.err(idx, ErrorKind::Missing(Item::Val, name.clone())),
    }
  }
  for name in general.val_env.keys() {
    st.err(idx, ErrorKind::Extra(Item::Val, name.clone()));
  }
}

fn val_info_enrich(
  st: &mut St,
  general: ValInfo,
  specific: &ValInfo,
  name: &str_util::Name,
  idx: sml_hir::Idx,
) {
  generalizes(st, general.ty_scheme, &specific.ty_scheme, idx);
  if !general.id_status.same_kind_as(&specific.id_status)
    && !matches!(specific.id_status, IdStatus::Val)
  {
    st.err(idx, ErrorKind::WrongIdStatus(name.clone()));
  }
}

// uh... recursion schemes??

fn env_realize(subst: &TyRealization, env: &mut Env) {
  for env in env.str_env.values_mut() {
    env_realize(subst, env);
  }
  for ty_info in env.ty_env.values_mut() {
    ty_realize(subst, &mut ty_info.ty_scheme.ty);
    val_env_realize(subst, &mut ty_info.val_env);
  }
  val_env_realize(subst, &mut env.val_env);
}

fn val_env_realize(subst: &TyRealization, val_env: &mut ValEnv) {
  for val_info in val_env.values_mut() {
    ty_realize(subst, &mut val_info.ty_scheme.ty);
  }
}

fn ty_realize(subst: &TyRealization, ty: &mut Ty) {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) => {}
    Ty::Record(rows) => {
      for ty in rows.values_mut() {
        ty_realize(subst, ty);
      }
    }
    Ty::Con(args, sym) => {
      for ty in args.iter_mut() {
        ty_realize(subst, ty);
      }
      if let Some(ty_scheme) = subst.get(sym) {
        // if this `if` does not hold, the sml is malformed, so let's just not proceed further.
        if ty_scheme.bound_vars.len() == args.len() {
          let mut ty_scheme_ty = ty_scheme.ty.clone();
          apply_bv(args, &mut ty_scheme_ty);
          *ty = ty_scheme_ty;
        }
      }
    }
    Ty::Fn(param, res) => {
      ty_realize(subst, param);
      ty_realize(subst, res);
    }
  }
}

fn bs_syms<F: FnMut(Sym)>(f: &mut F, bs: &Bs) {
  for fun_sig in bs.fun_env.values() {
    sig_syms(f, &fun_sig.param);
    for &sym in &fun_sig.body_ty_names {
      f(sym);
    }
    env_syms(f, &fun_sig.body_env);
  }
  for sig in bs.sig_env.values() {
    sig_syms(f, sig);
  }
  env_syms(f, &bs.env);
}

fn sig_syms<F: FnMut(Sym)>(f: &mut F, sig: &Sig) {
  for &sym in &sig.ty_names {
    f(sym);
  }
  env_syms(f, &sig.env);
}

fn env_syms<F, E>(f: &mut F, env: &E)
where
  F: FnMut(Sym),
  E: EnvLike,
{
  for env in env.all_str() {
    env_syms(f, env);
  }
  for ty_info in env.all_ty() {
    ty_syms(f, &ty_info.ty_scheme.ty);
    val_env_syms(f, &ty_info.val_env);
  }
  for val_info in env.all_val() {
    ty_syms(f, &val_info.ty_scheme.ty);
  }
}

fn val_env_syms<F: FnMut(Sym)>(f: &mut F, val_env: &ValEnv) {
  for val_info in val_env.values() {
    ty_syms(f, &val_info.ty_scheme.ty);
  }
}

fn assert_ty_has_no_record_meta_vars(x: Result<(), HasRecordMetaVars>) {
  x.expect("a type cannot have record meta vars because it has no patterns");
}
