use crate::error::{ErrorKind, Item};
use crate::st::St;
use crate::types::{
  generalize, generalize_fixed, Cx, Env, FixedTyVars, IdStatus, Ty, TyEnv, TyInfo, TyScheme,
  ValEnv, ValInfo,
};
use crate::unify::unify;
use crate::util::{apply, get_env, get_ty_info, ins_check_name, ins_no_dupe};
use crate::{exp, pat, ty};
use std::sync::Arc;

/// TODO avoid clones and have this take a &mut Cx instead, but promise that we won't actually
/// visibly mutate the cx between entry and exit (i.e. if we do any mutations, we'll undo them)?
pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, env: &mut Env, dec: hir::DecIdx) {
  let dec = match dec {
    Some(x) => x,
    None => return,
  };
  match &ars.dec[dec] {
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
      while let Some(val_bind) = val_binds.get(idx) {
        if val_bind.rec {
          // this and all other remaining ones are recursive.
          break;
        }
        idx += 1;
        // sml_def(25)
        let (pm_pat, mut want) = pat::get(st, &cx, ars, &mut ve, val_bind.pat);
        let got = exp::get(st, &cx, ars, val_bind.exp);
        unify(st, want.clone(), got, dec);
        apply(st.subst(), &mut want);
        pat::get_match(
          st,
          vec![pm_pat],
          want,
          Some(ErrorKind::NonExhaustiveBinding),
          val_bind.pat.map_or(hir::Idx::from(dec), Into::into),
        );
      }
      // deal with the recursive ones. first do all the patterns so we can update the ValEnv. we
      // also need a separate recursive-only ValEnv.
      let mut rec_ve = ValEnv::default();
      let got_pats: Vec<_> = val_binds[idx..]
        .iter()
        .map(|val_bind| pat::get(st, &cx, ars, &mut rec_ve, val_bind.pat))
        .collect();
      // merge the recursive and non-recursive ValEnvs, making sure they don't clash.
      for (name, val_info) in rec_ve.iter() {
        if let Some(e) = ins_no_dupe(&mut ve, name.clone(), val_info.clone(), Item::Val) {
          st.err(dec, e);
        }
      }
      // extend the cx with only the recursive ValEnv.
      Arc::make_mut(&mut cx.env).val_env.extend(rec_ve);
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
        pat::get_match(
          st,
          vec![pm_pat],
          want,
          Some(ErrorKind::NonExhaustiveBinding),
          dec,
        );
      }
      // generalize the entire merged ValEnv.
      for val_info in ve.values_mut() {
        generalize(st.subst(), fixed.clone(), &mut val_info.ty_scheme);
      }
      // extend the overall env with that.
      env.val_env.extend(ve);
    }
    // sml_def(16)
    hir::Dec::Ty(ty_binds) => {
      let mut cx = cx.clone();
      let mut ty_env = TyEnv::default();
      // sml_def(27)
      for ty_bind in ty_binds {
        let fixed = add_fixed_ty_vars(st, &mut cx, &ty_bind.ty_vars, dec.into());
        let mut ty_scheme = TyScheme::zero(ty::get(st, &cx, ars, ty_bind.ty));
        // use `generalize_fixed`, not `generalize`, to explicitly create a ty scheme with the
        // written arity to support phantom types.
        generalize_fixed(fixed, &mut ty_scheme);
        let ty_info = TyInfo {
          ty_scheme,
          val_env: ValEnv::default(),
        };
        if let Some(e) = ins_no_dupe(&mut ty_env, ty_bind.name.clone(), ty_info, Item::Ty) {
          st.err(dec, e)
        }
        for ty_var in ty_bind.ty_vars.iter() {
          cx.ty_vars.remove(ty_var);
        }
      }
      env.ty_env.extend(ty_env);
    }
    // sml_def(17)
    hir::Dec::Datatype(dat_binds) => {
      let (ty_env, big_val_env) = get_dat_binds(st, cx.clone(), ars, dat_binds, dec.into());
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
    hir::Dec::Abstype(_, _) => st.err(dec, ErrorKind::Unsupported("`abstype` declarations")),
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
            };
            if let Some(e) = ins_check_name(&mut val_env, name.clone(), vi, Item::Val) {
              st.err(dec, e);
            }
          }
          // sml_def(31)
          hir::ExBind::Copy(name, path) => match get_env(&cx.env, path.structures()) {
            Ok(got_env) => match got_env.val_env.get(path.last()) {
              Some(val_info) => match val_info.id_status {
                IdStatus::Exn(_) => {
                  match ins_no_dupe(&mut val_env, name.clone(), val_info.clone(), Item::Val) {
                    Some(e) => st.err(dec, e),
                    None => {}
                  }
                }
                _ => st.err(dec, ErrorKind::ExnCopyNotExnIdStatus),
              },
              None => st.err(dec, ErrorKind::Undefined(Item::Val, path.last().clone())),
            },
            Err(name) => st.err(dec, ErrorKind::Undefined(Item::Struct, name.clone())),
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
      Arc::make_mut(&mut cx.env).extend(local_env);
      get(st, &cx, ars, env, *in_dec);
    }
    // sml_def(22)
    hir::Dec::Open(paths) => {
      for path in paths {
        match get_env(&cx.env, path.all_names()) {
          Ok(got_env) => env.extend(got_env.clone()),
          Err(name) => st.err(dec, ErrorKind::Undefined(Item::Struct, name.clone())),
        }
      }
    }
    // sml_def(23), sml_def(24)
    hir::Dec::Seq(decs) => {
      let mut cx = cx.clone();
      for &dec in decs {
        let mut one_env = Env::default();
        get(st, &cx, ars, &mut one_env, dec);
        Arc::make_mut(&mut cx.env).extend(one_env.clone());
        env.extend(one_env);
      }
    }
  }
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
    if cx.ty_vars.insert(ty_var.clone(), fv.clone()).is_some() {
      let e = ErrorKind::Duplicate(Item::TyVar, ty_var.clone().into_name());
      st.err(idx, e);
    }
    ret.insert(fv);
  }
  ret
}

/// TODO handle side conditions
pub(crate) fn get_dat_binds(
  st: &mut St,
  mut cx: Cx,
  ars: &hir::Arenas,
  dat_binds: &[hir::DatBind],
  idx: hir::Idx,
) -> (TyEnv, ValEnv) {
  let mut ty_env = TyEnv::default();
  let mut big_val_env = ValEnv::default();
  // sml_def(28), sml_def(81)
  for dat_bind in dat_binds {
    let started = st.syms.start(dat_bind.name.clone());
    let fixed = add_fixed_ty_vars(st, &mut cx, &dat_bind.ty_vars, idx);
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
    // allow recursive reference
    Arc::make_mut(&mut cx.env).ty_env.insert(
      dat_bind.name.clone(),
      TyInfo {
        ty_scheme: ty_scheme.clone(),
        val_env: ValEnv::default(),
      },
    );
    let mut val_env = ValEnv::default();
    // sml_def(29), sml_def(82)
    for con_bind in dat_bind.cons.iter() {
      let mut ty = out_ty.clone();
      if let Some(of_ty) = con_bind.ty {
        ty = Ty::fun(ty::get(st, &cx, ars, of_ty), ty);
      };
      let mut ty_scheme = TyScheme::zero(ty);
      // just `generalize` would also work, because `ty_scheme` contains `out_ty`, which mentions
      // every fixed var.
      generalize_fixed(fixed.clone(), &mut ty_scheme);
      let vi = ValInfo {
        ty_scheme,
        id_status: IdStatus::Con,
      };
      if let Some(e) = ins_check_name(&mut val_env, con_bind.name.clone(), vi, Item::Val) {
        st.err(idx, e);
      }
    }
    // NOTE: no checking for duplicates here
    big_val_env.extend(val_env.iter().map(|(a, b)| (a.clone(), b.clone())));
    let ty_info = TyInfo { ty_scheme, val_env };
    st.syms.finish(started, ty_info.clone());
    if let Some(e) = ins_no_dupe(&mut ty_env, dat_bind.name.clone(), ty_info, Item::Ty) {
      st.err(idx, e);
    }
    for ty_var in dat_bind.ty_vars.iter() {
      cx.ty_vars.remove(ty_var);
    }
  }
  (ty_env, big_val_env)
}
