//! Check patterns.

use crate::ast::{Label, Pat as AstPat};
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::ck::ty;
use crate::statics::ck::util::{env_ins, env_merge, get_env, get_val_info, instantiate, tuple_lab};
use crate::statics::types::{Cx, Pat, Result, State, StaticsError, Ty, TyScheme, ValEnv, ValInfo};
use maplit::hashmap;
use std::collections::HashSet;

pub fn ck(cx: &Cx, st: &mut State, pat: &Located<AstPat<StrRef>>) -> Result<(ValEnv, Ty, Pat)> {
  let ret = match &pat.val {
    AstPat::Wildcard => (ValEnv::new(), Ty::Var(st.new_ty_var(false)), Pat::Anything),
    AstPat::DecInt(n) => (ValEnv::new(), Ty::INT, Pat::Int(*n)),
    AstPat::HexInt(n) => (ValEnv::new(), Ty::INT, Pat::Int(*n)),
    AstPat::DecWord(n) => (ValEnv::new(), Ty::WORD, Pat::Word(*n)),
    AstPat::HexWord(n) => (ValEnv::new(), Ty::WORD, Pat::Word(*n)),
    AstPat::String(s) => (ValEnv::new(), Ty::STRING, Pat::String(*s)),
    AstPat::Char(c) => (ValEnv::new(), Ty::CHAR, Pat::Char(*c)),
    AstPat::LongVid(vid) => {
      let ty_scheme = get_env(cx, vid)?
        .val_env
        .get(&vid.last.val)
        .and_then(|val_info| {
          if val_info.id_status.is_val() {
            None
          } else {
            Some(&val_info.ty_scheme)
          }
        });
      match ty_scheme {
        None => {
          // TODO should this be TyScheme::mono?
          let a = Ty::Var(st.new_ty_var(false));
          let val_info = ValInfo::val(TyScheme::mono(a.clone()));
          (hashmap![vid.last.val => val_info], a, Pat::Anything)
        }
        Some(ty_scheme) => {
          // TODO do we need to check ty_scheme yields a ConsType? e.g. `fn op:: => op::` may be
          // problematic
          let ty = instantiate(st, ty_scheme, pat.loc);
          (ValEnv::new(), ty, Pat::Ctor(vid.last.val, None))
        }
      }
    }
    AstPat::Record(rows, rest_loc) => {
      if let Some(loc) = rest_loc {
        return Err(loc.wrap(StaticsError::Todo));
      }
      let mut ve = ValEnv::new();
      let mut ty_rows = Vec::with_capacity(rows.len());
      let mut keys = HashSet::with_capacity(rows.len());
      let mut pat_rows = Vec::with_capacity(rows.len());
      for row in rows {
        let (other_ve, ty, pat) = ck(cx, st, &row.pat)?;
        if !keys.insert(row.lab.val) {
          return Err(row.lab.loc.wrap(StaticsError::DuplicateLabel(row.lab.val)));
        }
        env_merge(&mut ve, other_ve, row.pat.loc)?;
        ty_rows.push((row.lab.val, ty));
        pat_rows.push((row.lab.val, pat));
      }
      (ve, Ty::Record(ty_rows), Pat::Record(pat_rows))
    }
    AstPat::Tuple(pats) => {
      let mut ve = ValEnv::new();
      let mut ty_rows = Vec::with_capacity(pats.len());
      let mut pat_rows = Vec::with_capacity(pats.len());
      for (idx, pat) in pats.iter().enumerate() {
        let (other_ve, ty, new_pat) = ck(cx, st, pat)?;
        let lab = tuple_lab(idx);
        env_merge(&mut ve, other_ve, pat.loc)?;
        ty_rows.push((lab, ty));
        pat_rows.push((lab, new_pat));
      }
      (ve, Ty::Record(ty_rows), Pat::Record(pat_rows))
    }
    AstPat::List(pats) => {
      let elem = Ty::Var(st.new_ty_var(false));
      let mut ve = ValEnv::new();
      let mut new_pats = Vec::with_capacity(pats.len());
      for pat in pats {
        let (other_ve, ty, new_pat) = ck(cx, st, pat)?;
        env_merge(&mut ve, other_ve, pat.loc)?;
        st.subst.unify(pat.loc, elem.clone(), ty)?;
        new_pats.push(new_pat);
      }
      let pat = new_pats
        .into_iter()
        .rev()
        .fold(Pat::Ctor(StrRef::NIL, None), |ac, x| {
          Pat::Ctor(
            StrRef::CONS,
            Some(Pat::Record(vec![(Label::Num(1), x), (Label::Num(2), ac)]).into()),
          )
        });
      (ve, Ty::list(elem), pat)
    }
    AstPat::Ctor(vid, arg) => {
      let val_info = get_val_info(get_env(cx, vid)?, vid.last)?;
      if val_info.id_status.is_val() {
        return Err(vid.loc().wrap(StaticsError::ValAsPat));
      }
      let (val_env, arg_ty, arg_pat) = ck(cx, st, arg)?;
      let ctor_ty = instantiate(st, &val_info.ty_scheme, pat.loc);
      let ret_ty = Ty::Var(st.new_ty_var(false));
      let arrow_ty = Ty::Arrow(arg_ty.into(), ret_ty.clone().into());
      st.subst.unify(pat.loc, ctor_ty, arrow_ty)?;
      let pat = Pat::Ctor(vid.last.val, Some(arg_pat.into()));
      (val_env, ret_ty, pat)
    }
    AstPat::InfixCtor(lhs, vid, rhs) => {
      let val_info = get_val_info(&cx.env, *vid)?;
      if val_info.id_status.is_val() {
        return Err(vid.loc.wrap(StaticsError::ValAsPat));
      }
      let func_ty = instantiate(st, &val_info.ty_scheme, pat.loc);
      let (mut val_env, lhs_ty, lhs_pat) = ck(cx, st, lhs)?;
      let (other_ve, rhs_ty, rhs_pat) = ck(cx, st, rhs)?;
      env_merge(&mut val_env, other_ve, pat.loc)?;
      let ret_ty = Ty::Var(st.new_ty_var(false));
      let arrow_ty = Ty::Arrow(
        Ty::Record(vec![(Label::Num(1), lhs_ty), (Label::Num(2), rhs_ty)]).into(),
        ret_ty.clone().into(),
      );
      st.subst.unify(pat.loc, func_ty, arrow_ty)?;
      let pat = Pat::Ctor(
        vid.val,
        Some(Pat::Record(vec![(Label::Num(1), lhs_pat), (Label::Num(2), rhs_pat)]).into()),
      );
      (val_env, ret_ty, pat)
    }
    AstPat::Typed(inner_pat, ty) => {
      let (val_env, pat_ty, inner_pat) = ck(cx, st, inner_pat)?;
      let ty = ty::ck(cx, st, ty)?;
      st.subst.unify(pat.loc, ty, pat_ty.clone())?;
      (val_env, pat_ty, inner_pat)
    }
    AstPat::As(vid, ty, inner_pat) => {
      if cx
        .env
        .val_env
        .get(&vid.val)
        .map_or(false, |x| !x.id_status.is_val())
      {
        return Err(vid.loc.wrap(StaticsError::NonVarInAs(vid.val)));
      }
      let (mut val_env, pat_ty, inner_pat) = ck(cx, st, inner_pat)?;
      if let Some(ty) = ty {
        let ty = ty::ck(cx, st, ty)?;
        st.subst.unify(pat.loc, ty, pat_ty.clone())?;
      }
      let val_info = ValInfo::val(TyScheme::mono(pat_ty.clone()));
      env_ins(&mut val_env, *vid, val_info)?;
      (val_env, pat_ty, inner_pat)
    }
  };
  Ok(ret)
}
