use crate::error::{ErrorKind, Idx};
use crate::pat_match::Pat;
use crate::st::St;
use crate::types::{
  generalize, Cx, Env, FixedTyVars, IdStatus, Ty, TyInfo, TyScheme, ValEnv, ValInfo,
};
use crate::unify::unify;
use crate::util::apply;
use crate::{exp, pat, ty};

/// TODO avoid clones and have this take a &mut Cx instead, but promise that we won't actually
/// visibly mutate the cx between entry and exit (i.e. if we do any mutations, we'll undo them)?
pub(crate) fn get(st: &mut St, cx: &Cx, ars: &hir::Arenas, env: &mut Env, dec: hir::DecIdx) {
  let dec = match dec {
    Some(x) => x,
    None => return,
  };
  match &ars.dec[dec] {
    hir::Dec::Val(ty_vars, val_binds) => {
      let mut cx = cx.clone();
      let fixed = add_fixed_ty_vars(st, &mut cx, ty_vars);
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
        let (pm_pat, want) = pat::get(st, &cx, ars, &mut ve, val_bind.pat);
        get_val_exp(st, &cx, ars, val_bind.exp, pm_pat, want, dec.into());
      }
      // deal with the recursive ones. first do all the patterns so we can update the val env. we
      // also need a separate recursive-only val env.
      let mut rec_ve = ValEnv::default();
      let got_pats: Vec<_> = val_binds[idx..]
        .iter()
        .map(|val_bind| pat::get(st, &cx, ars, &mut rec_ve, val_bind.pat))
        .collect();
      // merge the recursive and non-recursive val envs, making sure they don't clash.
      for (name, val_info) in rec_ve.iter() {
        if ve.insert(name.clone(), val_info.clone()).is_some() {
          st.err(dec, ErrorKind::Redefined);
        }
      }
      // extend the cx with only the recursive val env.
      cx.env.val_env.extend(rec_ve);
      for (val_bind, (pm_pat, want)) in val_binds[idx..].iter().zip(got_pats) {
        if let Some(exp) = val_bind.exp {
          if !matches!(ars.exp[exp], hir::Exp::Fn(_)) {
            st.err(dec, ErrorKind::ValRecExpNotFn);
          }
        }
        get_val_exp(st, &cx, ars, val_bind.exp, pm_pat, want, dec.into());
      }
      // generalize the entire merged val env.
      for val_info in ve.values_mut() {
        generalize(st.subst(), fixed.clone(), &mut val_info.ty_scheme);
      }
      // extend the overall env with that.
      env.val_env.extend(ve);
      // clean up the ty vars.
      for ty_var in ty_vars.iter() {
        cx.ty_vars.remove(ty_var);
      }
    }
    hir::Dec::Ty(ty_binds) => {
      let mut cx = cx.clone();
      for ty_bind in ty_binds {
        let fixed = add_fixed_ty_vars(st, &mut cx, &ty_bind.ty_vars);
        let mut ty_scheme = TyScheme::mono(ty::get(st, &cx, ars, ty_bind.ty));
        generalize(st.subst(), fixed, &mut ty_scheme);
        let ty_info = TyInfo {
          ty_scheme,
          val_env: ValEnv::default(),
        };
        if env.ty_env.insert(ty_bind.name.clone(), ty_info).is_some() {
          st.err(dec, ErrorKind::Redefined)
        }
        for ty_var in ty_bind.ty_vars.iter() {
          cx.ty_vars.remove(ty_var);
        }
      }
    }
    hir::Dec::Datatype(dat_binds) => {
      let mut cx = cx.clone();
      for dat_bind in dat_binds {
        let fixed = add_fixed_ty_vars(st, &mut cx, &dat_bind.ty_vars);
        let dat = st.syms.start_datatype(dat_bind.name.clone());
        let out_ty = Ty::Con(
          fixed.iter().map(|x| Ty::FixedVar(x.clone())).collect(),
          dat.sym(),
        );
        let mut val_env = ValEnv::default();
        for con_bind in dat_bind.cons.iter() {
          let mut ty = out_ty.clone();
          if let Some(of_ty) = con_bind.ty {
            ty = Ty::Fn(ty::get(st, &cx, ars, of_ty).into(), ty.into());
          };
          let mut ty_scheme = TyScheme::mono(ty);
          generalize(st.subst(), fixed.clone(), &mut ty_scheme);
          let vi = ValInfo {
            ty_scheme,
            id_status: IdStatus::Con,
          };
          if val_env.insert(con_bind.name.clone(), vi).is_some() {
            st.err(dec, ErrorKind::Redefined);
          }
        }
        let mut ty_scheme = TyScheme::mono(out_ty);
        generalize(st.subst(), fixed, &mut ty_scheme);
        let ty_info = TyInfo { ty_scheme, val_env };
        st.syms.finish_datatype(dat, ty_info.clone());
        if env.ty_env.insert(dat_bind.name.clone(), ty_info).is_some() {
          st.err(dec, ErrorKind::Redefined);
        }
        for ty_var in dat_bind.ty_vars.iter() {
          cx.ty_vars.remove(ty_var);
        }
      }
    }
    hir::Dec::DatatypeCopy(_, _) => {
      // TODO
    }
    hir::Dec::Abstype(_, _) => {
      // TODO
    }
    hir::Dec::Exception(_) => {
      // TODO
    }
    hir::Dec::Local(_, _) => {
      // TODO
    }
    hir::Dec::Open(_) => {
      // TODO
    }
    hir::Dec::Seq(decs) => {
      for &dec in decs {
        get(st, cx, ars, env, dec);
      }
    }
  }
}

fn add_fixed_ty_vars(st: &mut St, cx: &mut Cx, ty_vars: &[hir::TyVar]) -> FixedTyVars {
  let mut ret = FixedTyVars::default();
  for ty_var in ty_vars.iter() {
    let fv = st.gen_fixed_var(ty_var.clone());
    // TODO shadowing? scoping?
    cx.ty_vars.insert(ty_var.clone(), fv.clone());
    ret.insert(fv);
  }
  ret
}

fn get_val_exp(
  st: &mut St,
  cx: &Cx,
  ars: &hir::Arenas,
  exp: hir::ExpIdx,
  pm_pat: Pat,
  mut want: Ty,
  idx: Idx,
) {
  let got = exp::get(st, cx, ars, exp);
  let idx = exp.map_or(idx, Into::into);
  unify(st, want.clone(), got, idx);
  apply(st.subst(), &mut want);
  pat::get_match(
    st,
    vec![pm_pat],
    want,
    Some(ErrorKind::NonExhaustiveBinding),
    idx,
  );
}
