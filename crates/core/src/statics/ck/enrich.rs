//! Check whether an environment enriches another environment.

use crate::loc::Loc;
use crate::statics::types::{
  Env, Error, Item, Result, Subst, SymSubst, TyFcn, TyInfo, TyScheme, Tys, ValEnv, ValInfo,
};

/// Returns `Ok(())` iff got enriches want (`got >> want`) as per the Definition. `loc` is the
/// location that errors will be wrapped in if this returns `Err(...)`, `tys` gives information
/// about types named by a `Sym`, and `sym_subst` is a substitution of symbols that is applied to
/// every `t`, where `t` is a `Ty` in `want`, before trying to unify `t` with its corresponding `Ty`
/// in `got`. TODO improve locs of errors.
pub fn ck(loc: Loc, tys: &Tys, sym_subst: &SymSubst, got: &Env, want: &Env) -> Result<()> {
  let cx = Cx {
    loc,
    tys,
    sym_subst,
  };
  ck_impl(cx, got, want)
}

/// The context, which we pass around to basically every function in this module.
#[derive(Clone, Copy)]
struct Cx<'t, 's> {
  /// The location, for errors.
  loc: Loc,
  /// The types.
  tys: &'t Tys,
  /// The symbol substitution, the type realization.
  ///
  /// TODO it's a little unpleasant that this, which is concerned with signature instantiation, is
  /// present here in this module, which is meant to be concerned with environment enrichment, since
  /// those problems are supposed to be orthogonal.
  ///
  /// But, to be fair, they are both used for the purpose of signature matching, and passing this
  /// down here means we don't have to do anything like first apply the substitution to the entire
  /// `want` env, which would require mutating the env and probably would mean we would have to
  /// create a bunch of new `Sym`s. In fact, I'm not even sure how I would do it. Though, this could
  /// be an indication that there is something fundamentally wrong with the approach of this
  /// implementation.
  sym_subst: &'s SymSubst,
}

fn ck_impl(cx: Cx, got: &Env, want: &Env) -> Result<()> {
  // For these for loops, we need the iteration order to be the same across different runs of the
  // program on the same file to ensure we get the same error message every time. This is useful for
  // testing and also is far less surprising for the user. Because of this need, the maps are
  // BTreeMaps, not HashMaps. See types.rs.
  for (name, want) in want.str_env.iter() {
    match got.str_env.get(name) {
      None => return Err(cx.loc.wrap(Error::Undefined(Item::Struct, *name))),
      Some(got) => ck_impl(cx, got, want)?,
    }
  }
  // Note that we do _not_ use the `SymSubst` in the `Cx` when looking up the `TyInfo`, since if we
  // did, we would be passing identical `TyInfo`s to `ck_ty_info`.
  for (name, want) in want.ty_env.inner.iter() {
    let want = cx.tys.get(want);
    match got.ty_env.inner.get(name) {
      None => return Err(cx.loc.wrap(Error::Undefined(Item::Ty, *name))),
      Some(got) => ck_ty_info(cx, cx.tys.get(got), want)?,
    }
  }
  for (name, want) in want.val_env.iter() {
    match got.val_env.get(name) {
      None => return Err(cx.loc.wrap(Error::Undefined(Item::Val, *name))),
      Some(got) => ck_val_info(cx, got, want)?,
    }
  }
  Ok(())
}

fn ck_val_info(cx: Cx, got: &ValInfo, want: &ValInfo) -> Result<()> {
  if want.id_status != got.id_status && !want.id_status.is_val() {
    // TODO improve this error to mention that it's also ok if want is a value?
    let err = Error::IdStatusMismatch(want.id_status, got.id_status);
    return Err(cx.loc.wrap(err));
  }
  ck_generalizes(cx, want.ty_scheme.clone(), got.ty_scheme.clone())?;
  Ok(())
}

fn ck_ty_info(cx: Cx, got: &TyInfo, want: &TyInfo) -> Result<()> {
  ck_ty_fcn_eq(cx, &got.ty_fcn, &want.ty_fcn)?;
  if want.val_env.is_empty() {
    return Ok(());
  }
  ck_val_env_eq(cx, &got.val_env, &want.val_env)
}

fn ck_val_env_eq(cx: Cx, got: &ValEnv, want: &ValEnv) -> Result<()> {
  let want_keys: Vec<_> = want.keys().copied().collect();
  let got_keys: Vec<_> = got.keys().copied().collect();
  if want_keys != got_keys {
    return Err(cx.loc.wrap(Error::ValEnvMismatch(want_keys, got_keys)));
  }
  for (name, want_vi) in want {
    let got_vi = got.get(name).unwrap();
    if want_vi.id_status != got_vi.id_status {
      let err = Error::IdStatusMismatch(want_vi.id_status, got_vi.id_status);
      return Err(cx.loc.wrap(err));
    }
    ck_ty_fcn_eq(cx, &got_vi.ty_scheme, &want_vi.ty_scheme)?;
  }
  Ok(())
}

/// Returns `Ok(())` if want = got. From the Definition: "It can be shown that lhs = rhs iff lhs >>>
/// rhs and rhs >>> lhs."
fn ck_ty_fcn_eq(cx: Cx, got: &TyFcn, want: &TyFcn) -> Result<()> {
  ck_generalizes(cx, want.clone(), got.clone())?;
  ck_generalizes(cx, got.clone(), want.clone())?;
  Ok(())
}

/// Returns Ok(s) iff want generalizes got as per the Definition, and s is this witness to this
/// fact. TODO is this right?
fn ck_generalizes(cx: Cx, mut want: TyScheme, mut got: TyScheme) -> Result<Subst> {
  let want_free_tvs = want.free_ty_vars();
  for tv in got.ty_vars.iter() {
    if want_free_tvs.contains(tv) {
      return Err(cx.loc.wrap(Error::Todo("bad free ty var")));
    }
  }
  want.ty.apply_sym_subst(cx.sym_subst);
  got.ty.apply_sym_subst(cx.sym_subst);
  let mut ret = Subst::default();
  ret.unify(cx.loc, &cx.tys, want.ty, got.ty)?;
  // TODO
  Ok(ret)
}
