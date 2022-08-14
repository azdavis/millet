use crate::error::{ErrorKind, Item};
use crate::get_env::{get_env_from_str_path, get_ty_info, get_val_info};
use crate::pat_match::Pat;
use crate::st::St;
use crate::types::{
  generalize, generalize_fixed, Cx, Env, EnvLike as _, FixedTyVars, HasRecordMetaVars, IdStatus,
  StartedSym, Ty, TyEnv, TyInfo, TyScheme, ValEnv, ValInfo,
};
use crate::unify::unify;
use crate::util::{apply, ins_check_name, ins_no_dupe};
use crate::{exp, pat, ty};
use fast_hash::FxHashMap;

pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, env: &mut Env, dec: hir::DecIdx) {
  let dec = match dec {
    Some(x) => x,
    None => return,
  };
  match &ars.dec[dec] {
    hir::Dec::Hole => st.err(dec, ErrorKind::DecHole),
    // sml_def(15)
    hir::Dec::Val(ty_vars, val_binds) => {
      let mut cx = cx.clone();
      let fixed = add_fixed_ty_vars(st, &mut cx, ty_vars, dec.into());
      // we actually resort to indexing logic because this is a little weird:
      // - we represent the recursive nature of ValBinds (and all other things that recurse with
      //   `and`) as a sequence of non-recursive items.
      // - if a ValBind is `rec`, it's not just this one, it's all the rest of them.
      // - we need to go over the recursive ValBinds twice.
      let mut idx = 0usize;
      let mut ve = ValEnv::default();
      let mut src_exp = FxHashMap::<hir::Name, hir::ExpIdx>::default();
      while let Some(val_bind) = val_binds.get(idx) {
        if val_bind.rec {
          // this and all other remaining ones are recursive.
          break;
        }
        idx += 1;
        // sml_def(25)
        let (pm_pat, mut want) = get_pat_and_src_exp(st, &cx, ars, &mut ve, val_bind, &mut src_exp);
        let got = exp::get(st, &cx, ars, val_bind.exp);
        unify(st, want.clone(), got, dec);
        apply(st.subst(), &mut want);
        st.insert_bind(
          pm_pat,
          want,
          val_bind.pat.map_or(hir::Idx::from(dec), Into::into),
        );
      }
      // deal with the recursive ones. first do all the patterns so we can update the ValEnv. we
      // also need a separate recursive-only ValEnv.
      let mut rec_ve = ValEnv::default();
      let got_pats: Vec<_> = val_binds[idx..]
        .iter()
        .map(|val_bind| get_pat_and_src_exp(st, &cx, ars, &mut rec_ve, val_bind, &mut src_exp))
        .collect();
      // merge the recursive and non-recursive ValEnvs, making sure they don't clash.
      for (name, val_info) in rec_ve.iter() {
        if let Some(e) = ins_no_dupe(&mut ve, name.clone(), val_info.clone(), Item::Val) {
          st.err(dec, e);
        }
      }
      // extend the cx with only the recursive ValEnv.
      cx.env.push(Env {
        val_env: rec_ve,
        ..Default::default()
      });
      for (val_bind, (pm_pat, mut want)) in val_binds[idx..].iter().zip(got_pats) {
        // sml_def(26)
        if let Some(exp) = val_bind.exp {
          if !matches!(ars.exp[exp], hir::Exp::Fn(_)) {
            st.err(dec, ErrorKind::ValRecExpNotFn);
          }
        }
        let got = exp::get(st, &cx, ars, val_bind.exp);
        unify(st, want.clone(), got, dec);
        apply(st.subst(), &mut want);
        st.insert_bind(pm_pat, want, dec.into());
      }
      // generalize the entire merged ValEnv.
      for (name, val_info) in ve.iter_mut() {
        let &exp = src_exp
          .get(name)
          .expect("should have an exp for every bound name");
        let g = generalize(st.subst(), fixed.clone(), &mut val_info.ty_scheme);
        if expansive(&cx, ars, exp) && !val_info.ty_scheme.bound_vars.is_empty() {
          st.err(
            exp.map_or(hir::Idx::Dec(dec), hir::Idx::Exp),
            ErrorKind::BindPolymorphicExpansiveExp,
          );
        }
        if let Err(HasRecordMetaVars) = g {
          st.err(dec, ErrorKind::UnresolvedRecordTy);
        }
      }
      // extend the overall env with that.
      env.val_env.extend(ve);
    }
    // sml_def(16)
    hir::Dec::Ty(ty_binds) => {
      let mut cx = cx.clone();
      let mut ty_env = TyEnv::default();
      // sml_def(27)
      get_ty_binds(st, &mut cx, ars, &mut ty_env, ty_binds, dec.into());
      env.ty_env.extend(ty_env);
    }
    // sml_def(17)
    hir::Dec::Datatype(dat_binds, ty_binds) => {
      let (ty_env, big_val_env) =
        get_dat_binds(st, cx.clone(), ars, dat_binds, ty_binds, dec.into());
      env.ty_env.extend(ty_env);
      env.val_env.extend(big_val_env);
    }
    // sml_def(18)
    hir::Dec::DatatypeCopy(name, path) => match get_ty_info(&cx.env, path) {
      Ok(ty_info) => {
        env.ty_env.insert(name.clone(), ty_info.clone());
        env
          .val_env
          .extend(ty_info.val_env.iter().map(|(a, b)| (a.clone(), b.clone())));
      }
      Err(e) => st.err(dec, e),
    },
    // sml_def(19)
    hir::Dec::Abstype(_, _, _) => st.err(dec, ErrorKind::Unsupported("`abstype` declarations")),
    // sml_def(20)
    hir::Dec::Exception(ex_binds) => {
      let mut val_env = ValEnv::default();
      for ex_bind in ex_binds {
        match ex_bind {
          // sml_def(30)
          hir::ExBind::New(name, param) => {
            let mut ty = Ty::EXN;
            let param = param.map(|param| ty::get(st, cx, ars, param));
            if let Some(ref param) = param {
              ty = Ty::fun(param.clone(), ty);
            }
            let exn = st.syms.insert_exn(name.clone(), param);
            let vi = ValInfo {
              ty_scheme: TyScheme::zero(ty),
              id_status: IdStatus::Exn(exn),
              def: st.def(dec),
            };
            if let Some(e) = ins_check_name(&mut val_env, name.clone(), vi, Item::Val) {
              st.err(dec, e);
            }
          }
          // sml_def(31)
          hir::ExBind::Copy(name, path) => match get_val_info(&cx.env, path) {
            Ok(Some(val_info)) => match val_info.id_status {
              IdStatus::Exn(_) => {
                match ins_no_dupe(&mut val_env, name.clone(), val_info.clone(), Item::Val) {
                  Some(e) => st.err(dec, e),
                  None => {}
                }
              }
              _ => st.err(dec, ErrorKind::ExnCopyNotExnIdStatus),
            },
            Ok(None) => st.err(dec, ErrorKind::Undefined(Item::Val, path.last().clone())),
            Err(e) => st.err(dec, e),
          },
        }
      }
      env.val_env.extend(val_env);
    }
    // sml_def(21)
    hir::Dec::Local(local_dec, in_dec) => {
      let mut local_env = Env::default();
      get(st, cx, ars, &mut local_env, *local_dec);
      let mut cx = cx.clone();
      cx.env.append(&mut local_env);
      get(st, &cx, ars, env, *in_dec);
    }
    // sml_def(22)
    hir::Dec::Open(paths) => {
      for path in paths {
        match get_env_from_str_path(&cx.env, path) {
          Ok(got_env) => env.append(&mut got_env.clone()),
          Err(e) => st.err(dec, e),
        }
      }
    }
    // sml_def(23), sml_def(24)
    hir::Dec::Seq(decs) => {
      let mut cx = cx.clone();
      let mut one_env = Env::default();
      for &dec in decs {
        get(st, &cx, ars, &mut one_env, dec);
        cx.env.push(one_env.clone());
        env.append(&mut one_env);
      }
    }
  }
}

fn get_pat_and_src_exp(
  st: &mut St,
  cx: &Cx,
  ars: &hir::Arenas,
  ve: &mut ValEnv,
  val_bind: &hir::ValBind,
  src_exp: &mut FxHashMap<hir::Name, hir::ExpIdx>,
) -> (Pat, Ty) {
  let ret = pat::get(st, cx, ars, ve, val_bind.pat);
  for name in ve.keys() {
    if !src_exp.contains_key(name) {
      src_exp.insert(name.clone(), val_bind.exp);
    }
  }
  ret
}

pub(crate) fn add_fixed_ty_vars(
  st: &mut St,
  cx: &mut Cx,
  ty_vars: &[hir::TyVar],
  idx: hir::Idx,
) -> FixedTyVars {
  let mut ret = FixedTyVars::default();
  for ty_var in ty_vars.iter() {
    let fv = st.gen_fixed_var(ty_var.clone());
    if cx.fixed.insert(ty_var.clone(), fv.clone()).is_some() {
      let e = ErrorKind::Duplicate(Item::TyVar, ty_var.as_name().clone());
      st.err(idx, e);
    }
    ret.insert(fv);
  }
  ret
}

fn get_ty_binds(
  st: &mut St,
  cx: &mut Cx,
  ars: &hir::Arenas,
  ty_env: &mut TyEnv,
  ty_binds: &[hir::TyBind],
  idx: hir::Idx,
) {
  for ty_bind in ty_binds {
    let fixed = add_fixed_ty_vars(st, cx, &ty_bind.ty_vars, idx);
    let mut ty_scheme = TyScheme::zero(ty::get(st, cx, ars, ty_bind.ty));
    generalize_fixed(fixed, &mut ty_scheme);
    let ty_info = TyInfo {
      ty_scheme,
      val_env: ValEnv::default(),
      def: st.def(idx),
    };
    if let Some(e) = ins_no_dupe(ty_env, ty_bind.name.clone(), ty_info, Item::Ty) {
      st.err(idx, e)
    }
    for ty_var in ty_bind.ty_vars.iter() {
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

/// TODO handle side conditions
pub(crate) fn get_dat_binds(
  st: &mut St,
  mut cx: Cx,
  ars: &hir::Arenas,
  dat_binds: &[hir::DatBind],
  ty_binds: &[hir::TyBind],
  idx: hir::Idx,
) -> (TyEnv, ValEnv) {
  // 'fake' because it's just to allow recursive reference, it doesn't have the val env filled in
  // with the constructors.
  let mut fake_ty_env = TyEnv::default();
  let mut datatypes = Vec::<Datatype>::new();
  // do a first pass through the datatypes to allow for recursive reference, and to put them in
  // scope for the types.
  for dat_bind in dat_binds.iter() {
    let started = st.syms.start(dat_bind.name.clone());
    // just create the fixed ty vars, do not bring them into the scope of the cx yet.
    let mut fixed = FixedTyVars::default();
    for ty_var in dat_bind.ty_vars.iter() {
      fixed.insert(st.gen_fixed_var(ty_var.clone()));
    }
    let out_ty = Ty::Con(
      fixed.iter().map(|x| Ty::FixedVar(x.clone())).collect(),
      started.sym(),
    );
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
    };
    if let Some(e) = ins_no_dupe(&mut fake_ty_env, dat_bind.name.clone(), ty_info, Item::Ty) {
      st.err(idx, e);
    }
    datatypes.push(Datatype {
      started,
      fixed,
      out_ty,
      ty_scheme,
    });
  }
  // bring all the datatypes into scope.
  cx.env.push(Env {
    ty_env: fake_ty_env.clone(),
    ..Default::default()
  });
  // now get the types. this `ty_env` will be the ultimate one we return.
  let mut ty_env = TyEnv::default();
  get_ty_binds(st, &mut cx, ars, &mut ty_env, ty_binds, idx);
  // make sure the types did not conflict with the datatypes.
  for (name, val) in ty_env.iter() {
    if let Some(e) = ins_no_dupe(&mut fake_ty_env, name.clone(), val.clone(), Item::Ty) {
      st.err(idx, e);
    }
  }
  // bring all the types into scope.
  cx.env.push(Env {
    ty_env: ty_env.clone(),
    ..Default::default()
  });
  // datatypes and types are now in scope. now get the datatype constructors.
  let mut big_val_env = ValEnv::default();
  assert_eq!(
    dat_binds.len(),
    datatypes.len(),
    "we created datatypes from a for loop over dat_binds"
  );
  // sml_def(28), sml_def(81)
  for (dat_bind, datatype) in dat_binds.iter().zip(datatypes) {
    // bring the type variables for this datatype into scope.
    for fv in datatype.fixed.iter() {
      if cx.fixed.insert(fv.ty_var().clone(), fv.clone()).is_some() {
        let e = ErrorKind::Duplicate(Item::TyVar, fv.ty_var().as_name().clone());
        st.err(idx, e);
      }
    }
    let mut val_env = ValEnv::default();
    // sml_def(29), sml_def(82)
    for con_bind in dat_bind.cons.iter() {
      let mut ty = datatype.out_ty.clone();
      if let Some(of_ty) = con_bind.ty {
        ty = Ty::fun(ty::get(st, &cx, ars, of_ty), ty);
      };
      let mut ty_scheme = TyScheme::zero(ty);
      // just `generalize` would also work, because `ty_scheme` contains `out_ty`, which mentions
      // every fixed var.
      generalize_fixed(datatype.fixed.clone(), &mut ty_scheme);
      let vi = ValInfo {
        ty_scheme,
        id_status: IdStatus::Con,
        def: st.def(idx),
      };
      if let Some(e) = ins_check_name(&mut val_env, con_bind.name.clone(), vi, Item::Val) {
        st.err(idx, e);
      }
    }
    // NOTE: no checking for duplicates here
    big_val_env.extend(val_env.iter().map(|(a, b)| (a.clone(), b.clone())));
    let ty_info = TyInfo {
      ty_scheme: datatype.ty_scheme,
      val_env,
      def: st.def(idx),
    };
    st.syms.finish(datatype.started, ty_info.clone());
    ty_env.insert(dat_bind.name.clone(), ty_info);
    for ty_var in dat_bind.ty_vars.iter() {
      cx.fixed.remove(ty_var);
    }
  }
  (ty_env, big_val_env)
}

fn expansive(cx: &Cx, ars: &hir::Arenas, exp: hir::ExpIdx) -> bool {
  let exp = match exp {
    Some(x) => x,
    None => return false,
  };
  match &ars.exp[exp] {
    hir::Exp::Hole | hir::Exp::SCon(_) | hir::Exp::Path(_) | hir::Exp::Fn(_) => false,
    hir::Exp::Let(_, _) | hir::Exp::Raise(_) | hir::Exp::Handle(_, _) => true,
    hir::Exp::Record(rows) => rows.iter().any(|&(_, exp)| expansive(cx, ars, exp)),
    hir::Exp::App(func, arg) => !constructor(cx, ars, *func) || expansive(cx, ars, *arg),
    hir::Exp::Typed(exp, _) => expansive(cx, ars, *exp),
  }
}

/// this will sometimes return true for expressions that could not possibly be a "constructor" in
/// the sense of section 4.7 of the definition, like records. but there would have been a type error
/// emitted for the application already anyway.
fn constructor(cx: &Cx, ars: &hir::Arenas, exp: hir::ExpIdx) -> bool {
  let exp = match exp {
    Some(x) => x,
    None => return true,
  };
  match &ars.exp[exp] {
    hir::Exp::Hole | hir::Exp::SCon(_) => true,
    hir::Exp::Let(_, _)
    | hir::Exp::App(_, _)
    | hir::Exp::Handle(_, _)
    | hir::Exp::Raise(_)
    | hir::Exp::Fn(_) => false,
    hir::Exp::Record(rows) => rows.iter().any(|&(_, exp)| constructor(cx, ars, exp)),
    hir::Exp::Typed(exp, _) => constructor(cx, ars, *exp),
    hir::Exp::Path(path) => {
      if path.structures().is_empty() && path.last().as_str() == "ref" {
        return false;
      }
      match get_val_info(&cx.env, path) {
        Ok(Some(x)) => matches!(x.id_status, IdStatus::Con | IdStatus::Exn(_)),
        Ok(None) | Err(_) => true,
      }
    }
  }
}
