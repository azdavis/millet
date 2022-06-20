use crate::error::{ErrorKind, Item};
use crate::fmt_util::ty_var_name;
use crate::st::St;
use crate::types::{
  generalize, Bs, Env, FunEnv, FunSig, IdStatus, Sig, SigEnv, StrEnv, Sym, Ty, TyEnv, TyInfo,
  TyNameSet, TyScheme, TyVarKind, ValEnv, ValInfo,
};
use crate::util::{
  apply_bv, get_env, get_ty_info, get_ty_info_raw, ins_check_name, ins_no_dupe, instantiate,
};
use crate::{dec, ty, unify::unify};
use fast_hash::{map, FxHashMap, FxHashSet};
use std::sync::Arc;

pub(crate) fn get(st: &mut St, bs: &mut Bs, ars: &hir::Arenas, top_dec: hir::TopDecIdx) {
  match &ars.top_dec[top_dec] {
    // sml_def(87)
    hir::TopDec::Str(str_dec) => {
      let mut env = Env::default();
      get_str_dec(st, bs, ars, &mut env, *str_dec);
      Arc::make_mut(&mut bs.env).extend(env);
    }
    // sml_def(66), sml_def(88)
    hir::TopDec::Sig(sig_binds) => {
      let mut sig_env = SigEnv::default();
      // sml_def(67)
      for sig_bind in sig_binds {
        let mut env = Env::default();
        get_sig_exp(st, bs, ars, &mut env, sig_bind.sig_exp);
        let sig = env_to_sig(bs, env);
        if let Some(e) = ins_no_dupe(&mut sig_env, sig_bind.name.clone(), sig, Item::Sig) {
          st.err(top_dec, e);
        }
      }
      bs.sig_env.extend(sig_env);
    }
    // sml_def(85), sml_def(89)
    hir::TopDec::Functor(fun_binds) => {
      let mut fun_env = FunEnv::default();
      // sml_def(86)
      for fun_bind in fun_binds {
        let mut param_env = Env::default();
        get_sig_exp(st, bs, ars, &mut param_env, fun_bind.param_sig);
        let param_sig = env_to_sig(bs, param_env);
        let mut bs_clone = bs.clone();
        Arc::make_mut(&mut bs_clone.env)
          .str_env
          .insert(fun_bind.param_name.clone(), param_sig.env.clone());
        let mut body_env = Env::default();
        get_str_exp(st, &bs_clone, ars, &mut body_env, fun_bind.body);
        let mut body_ty_names = TyNameSet::default();
        env_syms(&mut |x| ignore(body_ty_names.insert(x)), &body_env);
        bs_syms(&mut |x| ignore(body_ty_names.remove(&x)), &bs_clone);
        for sym in param_sig.ty_names.iter() {
          body_ty_names.remove(sym);
        }
        let fun_name = fun_bind.functor_name.clone();
        let fun_sig = FunSig {
          param: param_sig,
          res: Sig {
            ty_names: body_ty_names,
            env: body_env,
          },
        };
        if let Some(e) = ins_no_dupe(&mut fun_env, fun_name, fun_sig, Item::Functor) {
          st.err(top_dec, e);
        }
      }
      bs.fun_env.extend(fun_env);
    }
  }
}

fn get_str_exp(st: &mut St, bs: &Bs, ars: &hir::Arenas, env: &mut Env, str_exp: hir::StrExpIdx) {
  let str_exp = match str_exp {
    Some(x) => x,
    None => return,
  };
  match &ars.str_exp[str_exp] {
    // sml_def(50)
    hir::StrExp::Struct(str_dec) => get_str_dec(st, bs, ars, env, *str_dec),
    // sml_def(51)
    hir::StrExp::Path(path) => match get_env(&bs.env, path.all_names()) {
      Ok(got_env) => env.extend(got_env.clone()),
      Err(name) => st.err(str_exp, ErrorKind::Undefined(Item::Struct, name.clone())),
    },
    // sml_def(52), sml_def(53)
    hir::StrExp::Ascription(inner_str_exp, asc, sig_exp) => {
      let mut str_exp_env = Env::default();
      get_str_exp(st, bs, ars, &mut str_exp_env, *inner_str_exp);
      let mut sig_exp_env = Env::default();
      get_sig_exp(st, bs, ars, &mut sig_exp_env, *sig_exp);
      let sig = env_to_sig(bs, sig_exp_env);
      let mut subst = TyRealization::default();
      let mut to_extend = sig.env.clone();
      if st.mode().is_regular() {
        env_instance_sig(st, &mut subst, &str_exp_env, &sig, str_exp.into());
        env_realize(&subst, &mut to_extend);
        env_enrich(st, &str_exp_env, &to_extend, str_exp.into());
      }
      if matches!(asc, hir::Ascription::Opaque) {
        subst.clear();
        gen_fresh_syms(st, &mut subst, &sig.ty_names);
        to_extend = sig.env.clone();
        env_realize(&subst, &mut to_extend);
      }
      env.extend(to_extend);
    }
    // sml_def(54)
    hir::StrExp::App(fun_name, arg_str_exp) => match bs.fun_env.get(fun_name) {
      Some(fun_sig) => {
        let mut arg_env = Env::default();
        get_str_exp(st, bs, ars, &mut arg_env, *arg_str_exp);
        let mut subst = TyRealization::default();
        let mut to_extend = fun_sig.res.env.clone();
        let arg_idx = hir::Idx::from(arg_str_exp.unwrap_or(str_exp));
        env_instance_sig(st, &mut subst, &arg_env, &fun_sig.param, arg_idx);
        gen_fresh_syms(st, &mut subst, &fun_sig.res.ty_names);
        env_realize(&subst, &mut to_extend);
        let mut param_env = fun_sig.param.env.clone();
        env_realize(&subst, &mut param_env);
        env_enrich(st, &arg_env, &param_env, arg_idx);
        env.extend(to_extend);
      }
      None => st.err(
        str_exp,
        ErrorKind::Undefined(Item::Functor, fun_name.clone()),
      ),
    },
    // sml_def(55)
    hir::StrExp::Let(str_dec, str_exp) => {
      let mut let_env = Env::default();
      get_str_dec(st, bs, ars, &mut let_env, *str_dec);
      let mut bs = bs.clone();
      Arc::make_mut(&mut bs.env).extend(let_env);
      get_str_exp(st, &bs, ars, env, *str_exp)
    }
  }
}

fn get_str_dec(st: &mut St, bs: &Bs, ars: &hir::Arenas, env: &mut Env, str_dec: hir::StrDecIdx) {
  let str_dec = match str_dec {
    Some(x) => x,
    None => return,
  };
  match &ars.str_dec[str_dec] {
    // sml_def(56)
    hir::StrDec::Dec(dec) => dec::get(st, &bs.as_cx(), ars, env, *dec),
    // sml_def(57)
    hir::StrDec::Structure(str_binds) => {
      // sml_def(61)
      let mut str_env = StrEnv::default();
      for str_bind in str_binds {
        let mut env = Env::default();
        get_str_exp(st, bs, ars, &mut env, str_bind.str_exp);
        if let Some(e) = ins_no_dupe(&mut str_env, str_bind.name.clone(), env, Item::Struct) {
          st.err(str_dec, e);
        }
      }
      env.str_env.extend(str_env);
    }
    // sml_def(58)
    hir::StrDec::Local(local_dec, in_dec) => {
      let mut local_env = Env::default();
      get_str_dec(st, bs, ars, &mut local_env, *local_dec);
      let mut bs = bs.clone();
      Arc::make_mut(&mut bs.env).extend(local_env);
      get_str_dec(st, &bs, ars, env, *in_dec);
    }
    // sml_def(59), sml_def(60)
    hir::StrDec::Seq(str_decs) => {
      let mut bs = bs.clone();
      for &str_dec in str_decs {
        let mut one_env = Env::default();
        get_str_dec(st, &bs, ars, &mut one_env, str_dec);
        Arc::make_mut(&mut bs.env).extend(one_env.clone());
        env.extend(one_env);
      }
    }
  }
}

fn get_sig_exp(st: &mut St, bs: &Bs, ars: &hir::Arenas, env: &mut Env, sig_exp: hir::SigExpIdx) {
  let sig_exp = match sig_exp {
    Some(x) => x,
    None => return,
  };
  match &ars.sig_exp[sig_exp] {
    // sml_def(62)
    hir::SigExp::Spec(spec) => get_spec(st, bs, ars, env, *spec),
    // sml_def(63)
    hir::SigExp::Name(name) => match bs.sig_env.get(name) {
      Some(sig) => {
        let mut subst = TyRealization::default();
        gen_fresh_syms(st, &mut subst, &sig.ty_names);
        let mut sig_env = sig.env.clone();
        env_realize(&subst, &mut sig_env);
        env.extend(sig_env);
      }
      None => st.err(sig_exp, ErrorKind::Undefined(Item::Sig, name.clone())),
    },
    // sml_def(64)
    hir::SigExp::Where(inner, ty_vars, path, ty) => {
      let mut inner_env = Env::default();
      get_sig_exp(st, bs, ars, &mut inner_env, *inner);
      let mut cx = bs.as_cx();
      let fixed = dec::add_fixed_ty_vars(st, &mut cx, ty_vars, sig_exp.into());
      let mut ty_scheme = TyScheme::zero(ty::get(st, &cx, ars, *ty));
      generalize(st.subst(), fixed, &mut ty_scheme);
      match get_ty_info(&inner_env, path) {
        Ok(ty_info) => {
          let want_len = ty_info.ty_scheme.bound_vars.len();
          if want_len == ty_vars.len() {
            match &ty_info.ty_scheme.ty {
              Ty::None => {}
              // TODO side condition for sym not in T of B?
              Ty::Con(_, sym) => env_realize(&map([(*sym, ty_scheme)]), &mut inner_env),
              // TODO is this reachable?
              t => log::error!("reached an unexpected case: {t:?}"),
            }
          } else {
            st.err(sig_exp, ErrorKind::WrongNumTyArgs(want_len, ty_vars.len()));
          }
        }
        Err(e) => st.err(sig_exp, e),
      }
      env.extend(inner_env);
    }
  }
}

fn gen_fresh_syms(st: &mut St, subst: &mut TyRealization, ty_names: &TyNameSet) {
  let iter = ty_names.iter().map(|&sym| {
    let (name, ty_info) = st.syms.get(&sym).unwrap();
    let name = name.clone();
    let ty_info = ty_info.clone();
    let started = st.syms.start(name);
    let new_sym = started.sym();
    let ty_scheme = TyScheme::n_ary(ty_info.ty_scheme.bound_vars.kinds().copied(), new_sym);
    st.syms.finish(
      started,
      TyInfo {
        ty_scheme: ty_scheme.clone(),
        val_env: ValEnv::default(),
      },
    );
    (sym, ty_scheme)
  });
  subst.extend(iter);
}

// sml_def(65)
fn env_to_sig(bs: &Bs, env: Env) -> Sig {
  let mut ty_names = TyNameSet::default();
  env_syms(&mut |x| ignore(ty_names.insert(x)), &env);
  bs_syms(&mut |x| ignore(ty_names.remove(&x)), bs);
  Sig { ty_names, env }
}

fn get_spec(st: &mut St, bs: &Bs, ars: &hir::Arenas, env: &mut Env, spec: hir::SpecIdx) {
  let spec = match spec {
    Some(x) => x,
    None => return,
  };
  match &ars.spec[spec] {
    // sml_def(68)
    hir::Spec::Val(ty_vars, val_descs) => {
      // sml_def(79)
      let mut cx = bs.as_cx();
      let fixed = dec::add_fixed_ty_vars(st, &mut cx, ty_vars, spec.into());
      for val_desc in val_descs {
        let mut ty_scheme = TyScheme::zero(ty::get(st, &cx, ars, val_desc.ty));
        generalize(st.subst(), fixed.clone(), &mut ty_scheme);
        let vi = ValInfo {
          ty_scheme,
          id_status: IdStatus::Val,
        };
        let name = &val_desc.name;
        if let Some(e) = ins_check_name(&mut env.val_env, name.clone(), vi, Item::Val) {
          st.err(spec, e);
        }
      }
    }
    // sml_def(69). TODO check does not admit equality
    hir::Spec::Ty(ty_descs) => get_ty_desc(st, &mut env.ty_env, ty_descs, spec.into()),
    // sml_def(70). TODO check does admit equality
    hir::Spec::EqTy(ty_descs) => get_ty_desc(st, &mut env.ty_env, ty_descs, spec.into()),
    // sml_def(71)
    hir::Spec::Datatype(dat_desc) => {
      let dat_descs = std::slice::from_ref(dat_desc);
      let (ty_env, big_val_env) = dec::get_dat_binds(st, bs.as_cx(), ars, dat_descs, spec.into());
      for (name, val) in ty_env {
        if let Some(e) = ins_no_dupe(&mut env.ty_env, name, val, Item::Ty) {
          st.err(spec, e);
        }
      }
      for (name, val) in big_val_env {
        if let Some(e) = ins_no_dupe(&mut env.val_env, name, val, Item::Val) {
          st.err(spec, e);
        }
      }
    }
    // sml_def(72)
    hir::Spec::DatatypeCopy(name, path) => match get_ty_info(&bs.env, path) {
      Ok(ty_info) => {
        let ins = ins_no_dupe(&mut env.ty_env, name.clone(), ty_info.clone(), Item::Ty);
        if let Some(e) = ins {
          st.err(spec, e);
        }
        for (name, val_info) in ty_info.val_env.iter() {
          let ins = ins_no_dupe(&mut env.val_env, name.clone(), val_info.clone(), Item::Val);
          if let Some(e) = ins {
            st.err(spec, e);
          }
        }
      }
      Err(e) => st.err(spec, e),
    },
    // sml_def(73), sml_def(83)
    hir::Spec::Exception(ex_desc) => {
      let cx = bs.as_cx();
      // almost the same as the logic in dec::get, save for the check for ty vars.
      let mut ty = Ty::EXN;
      let param = ex_desc.ty.map(|param| ty::get(st, &cx, ars, param));
      if let Some(ref param) = param {
        ty = Ty::fun(param.clone(), ty);
      }
      let exn = st.syms.insert_exn(ex_desc.name.clone(), param);
      let vi = ValInfo {
        ty_scheme: TyScheme::zero(ty),
        id_status: IdStatus::Exn(exn),
      };
      if let Some(e) = ins_check_name(&mut env.val_env, ex_desc.name.clone(), vi, Item::Val) {
        st.err(spec, e);
      }
    }
    // sml_def(74), sml_def(84)
    hir::Spec::Str(str_desc) => {
      let mut one_env = Env::default();
      get_sig_exp(st, bs, ars, &mut one_env, str_desc.sig_exp);
      let name = str_desc.name.clone();
      if let Some(e) = ins_no_dupe(&mut env.str_env, name, one_env, Item::Struct) {
        st.err(spec, e);
      }
    }
    // sml_def(75)
    hir::Spec::Include(sig_exp) => get_sig_exp(st, bs, ars, env, *sig_exp),
    // sml_def(78)
    hir::Spec::Sharing(inner, kind, paths) => {
      let mut inner_env = Env::default();
      get_spec(st, bs, ars, &mut inner_env, *inner);
      match kind {
        hir::SharingKind::Regular => get_sharing_spec(st, &mut inner_env, paths, spec.into()),
        hir::SharingKind::Derived => {
          st.err(spec, ErrorKind::Unsupported("`sharing` spec derived form"));
        }
      }
      env.extend(inner_env);
    }
    // sml_def(76), sml_def(77)
    hir::Spec::Seq(specs) => {
      let mut bs = bs.clone();
      for &spec in specs {
        let mut one_env = Env::default();
        get_spec(st, &bs, ars, &mut one_env, spec);
        Arc::make_mut(&mut bs.env).extend(one_env.clone());
        env.extend(one_env);
      }
    }
  }
}

fn get_sharing_spec(st: &mut St, inner_env: &mut Env, paths: &[hir::Path], idx: hir::Idx) {
  let mut ty_scheme = None::<TyScheme>;
  let mut syms = Vec::<Sym>::with_capacity(paths.len());
  for path in paths {
    match get_ty_info(inner_env, path) {
      Ok(ty_info) => {
        // TODO assert exists a s.t. for all ty schemes, arity of ty scheme = a? and all other
        // things about the bound ty vars are 'compatible'? (the bit about admitting equality?)
        match &ty_info.ty_scheme.ty {
          Ty::None => {}
          // TODO side condition for sym not in T of B?
          Ty::Con(_, sym) => {
            if ty_scheme.is_none() {
              ty_scheme = Some(ty_info.ty_scheme.clone());
            }
            syms.push(*sym);
          }
          // TODO is this reachable?
          t => log::error!("reached an unexpected case: {t:?}"),
        }
      }
      Err(e) => st.err(idx, e),
    }
  }
  match ty_scheme {
    Some(ty_scheme) => {
      let subst: TyRealization = syms
        .into_iter()
        .map(|sym| (sym, ty_scheme.clone()))
        .collect();
      env_realize(&subst, inner_env);
    }
    None => log::error!("didn't get a ty scheme for sharing spec"),
  }
}

// sml_def(80). TODO equality checks
fn get_ty_desc(st: &mut St, ty_env: &mut TyEnv, ty_desc: &hir::TyDesc, idx: hir::Idx) {
  let mut ty_vars = FxHashSet::<&hir::TyVar>::default();
  let started = st.syms.start(ty_desc.name.clone());
  for ty_var in ty_desc.ty_vars.iter() {
    if !ty_vars.insert(ty_var) {
      let e = ErrorKind::Duplicate(Item::TyVar, ty_var.clone().into_name());
      st.err(idx, e);
    }
  }
  let ty_info = TyInfo {
    ty_scheme: TyScheme::n_ary(
      ty_desc
        .ty_vars
        .iter()
        .map(|x| x.is_equality().then(|| TyVarKind::Equality)),
      started.sym(),
    ),
    val_env: ValEnv::default(),
  };
  st.syms.finish(started, ty_info.clone());
  if let Some(e) = ins_no_dupe(ty_env, ty_desc.name.clone(), ty_info, Item::Ty) {
    st.err(idx, e);
  }
}

type TyRealization = FxHashMap<Sym, TyScheme>;

fn env_instance_sig(st: &mut St, subst: &mut TyRealization, env: &Env, sig: &Sig, idx: hir::Idx) {
  for &sym in sig.ty_names.iter() {
    let mut path = Vec::<&hir::Name>::new();
    if !bound_ty_name_to_path(&mut path, &sig.env, sym) {
      log::error!("couldn't get a path for {sym:?} in {sig:?}");
      continue;
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

fn bound_ty_name_to_path<'e>(ac: &mut Vec<&'e hir::Name>, env: &'e Env, sym: Sym) -> bool {
  for (name, ty_info) in env.ty_env.iter() {
    if let Ty::Con(_, s) = &ty_info.ty_scheme.ty {
      if *s == sym {
        ac.push(name);
        return true;
      }
    }
  }
  for (name, env) in env.str_env.iter() {
    ac.push(name);
    if bound_ty_name_to_path(ac, env, sym) {
      return true;
    }
    ac.pop();
  }
  false
}

// TODO for the enrich family of fns, improve error messages/range. for ranges, we might need to
// track `hir::Idx`es either in the Env or in a separate map with exactly the same keys (names). or
// we could add a special env only for use here that has the indices?

fn env_enrich(st: &mut St, general: &Env, specific: &Env, idx: hir::Idx) {
  for (name, specific) in specific.str_env.iter() {
    match general.str_env.get(name) {
      Some(general) => env_enrich(st, general, specific, idx),
      None => st.err(idx, ErrorKind::Missing(Item::Struct, name.clone())),
    }
  }
  for (name, specific) in specific.ty_env.iter() {
    match general.ty_env.get(name) {
      Some(general) => ty_info_enrich(st, general, specific, idx),
      None => st.err(idx, ErrorKind::Missing(Item::Ty, name.clone())),
    }
  }
  for (name, specific) in specific.val_env.iter() {
    match general.val_env.get(name) {
      Some(general) => val_info_enrich(st, general, specific, name, idx),
      None => st.err(idx, ErrorKind::Missing(Item::Val, name.clone())),
    }
  }
}

fn ty_info_enrich(st: &mut St, general: &TyInfo, specific: &TyInfo, idx: hir::Idx) {
  eq_ty_scheme(st, &general.ty_scheme, &specific.ty_scheme, idx);
  if specific.val_env.is_empty() {
    return;
  }
  let mut general_val_env = general.val_env.clone();
  for (name, specific) in specific.val_env.iter() {
    match general_val_env.remove(name) {
      Some(general) => {
        if !general.id_status.same_kind_as(&specific.id_status) {
          st.err(idx, ErrorKind::WrongIdStatus(name.clone()));
        }
        eq_ty_scheme(st, &general.ty_scheme, &specific.ty_scheme, idx);
      }
      None => st.err(idx, ErrorKind::Missing(Item::Val, name.clone())),
    }
  }
  for name in general_val_env.keys() {
    st.err(idx, ErrorKind::Extra(Item::Val, name.clone()));
  }
}

fn val_info_enrich(
  st: &mut St,
  general: &ValInfo,
  specific: &ValInfo,
  name: &hir::Name,
  idx: hir::Idx,
) {
  generalizes(st, &general.ty_scheme, &specific.ty_scheme, idx);
  if !general.id_status.same_kind_as(&specific.id_status)
    && !matches!(specific.id_status, IdStatus::Val)
  {
    st.err(idx, ErrorKind::WrongIdStatus(name.clone()));
  }
}

fn eq_ty_scheme(st: &mut St, lhs: &TyScheme, rhs: &TyScheme, idx: hir::Idx) {
  // TODO just use `==` since alpha equivalent ty schemes are already `==` for derive(PartialEq)?
  generalizes(st, lhs, rhs, idx);
  generalizes(st, rhs, lhs, idx);
}

fn generalizes(st: &mut St, general: &TyScheme, specific: &TyScheme, idx: hir::Idx) {
  let general = instantiate(st, general);
  let specific = {
    let subst: Vec<_> = specific
      .bound_vars
      .kinds()
      .enumerate()
      .map(|(idx, kind)| {
        let equality = matches!(kind, Some(TyVarKind::Equality));
        let ty_var: String = ty_var_name(equality, idx).collect();
        Ty::FixedVar(st.gen_fixed_var(hir::TyVar::new(ty_var)))
      })
      .collect();
    let mut ty = specific.ty.clone();
    apply_bv(&subst, &mut ty);
    ty
  };
  unify(st, specific, general, idx)
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
        assert_eq!(ty_scheme.bound_vars.len(), args.len());
        let mut ty_scheme_ty = ty_scheme.ty.clone();
        apply_bv(args, &mut ty_scheme_ty);
        *ty = ty_scheme_ty;
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
    sig_syms(f, &fun_sig.res);
  }
  for sig in bs.sig_env.values() {
    sig_syms(f, sig);
  }
  env_syms(f, &bs.env);
}

fn sig_syms<F: FnMut(Sym)>(f: &mut F, sig: &Sig) {
  for &sym in sig.ty_names.iter() {
    f(sym);
  }
  env_syms(f, &sig.env);
}

fn env_syms<F: FnMut(Sym)>(f: &mut F, env: &Env) {
  for env in env.str_env.values() {
    env_syms(f, env);
  }
  for ty_info in env.ty_env.values() {
    ty_syms(f, &ty_info.ty_scheme.ty);
    val_env_syms(f, &ty_info.val_env);
  }
  val_env_syms(f, &env.val_env);
}

fn val_env_syms<F: FnMut(Sym)>(f: &mut F, val_env: &ValEnv) {
  for val_info in val_env.values() {
    ty_syms(f, &val_info.ty_scheme.ty);
  }
}

fn ty_syms<F: FnMut(Sym)>(f: &mut F, ty: &Ty) {
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::MetaVar(_) | Ty::FixedVar(_) => {}
    Ty::Record(rows) => {
      for ty in rows.values() {
        ty_syms(f, ty);
      }
    }
    Ty::Con(args, sym) => {
      f(*sym);
      for ty in args {
        ty_syms(f, ty);
      }
    }
    Ty::Fn(param, res) => {
      ty_syms(f, param);
      ty_syms(f, res);
    }
  }
}

/// useful for closures that add/remove things from sets, since those methods return `bool`.
fn ignore(_: bool) {}
