//! Check patterns.

use crate::ast::{Label, Long, Pat as AstPat};
use crate::intern::StrRef;
use crate::loc::{Loc, Located};
use crate::statics::ck::ty;
use crate::statics::ck::util::{env_ins, env_merge, get_env, get_val_info, instantiate, tuple_lab};
use crate::statics::types::{
  Con, Cx, Error, Pat, Result, Span, State, Sym, SymTys, Ty, TyScheme, ValEnv, ValInfo,
};
use maplit::hashmap;
use std::collections::BTreeMap;

pub fn ck(cx: &Cx, st: &mut State, pat: &Located<AstPat<StrRef>>) -> Result<(ValEnv, Ty, Pat)> {
  let ret = match &pat.val {
    AstPat::Wildcard => (ValEnv::new(), Ty::Var(st.new_ty_var(false)), Pat::Anything),
    AstPat::DecInt(n) => (ValEnv::new(), Ty::INT, Pat::zero(Con::Int(*n))),
    AstPat::HexInt(n) => (ValEnv::new(), Ty::INT, Pat::zero(Con::Int(*n))),
    AstPat::DecWord(n) => (ValEnv::new(), Ty::WORD, Pat::zero(Con::Word(*n))),
    AstPat::HexWord(n) => (ValEnv::new(), Ty::WORD, Pat::zero(Con::Word(*n))),
    AstPat::String(s) => (ValEnv::new(), Ty::STRING, Pat::zero(Con::String(*s))),
    AstPat::Char(c) => (ValEnv::new(), Ty::CHAR, Pat::zero(Con::Char(*c))),
    AstPat::LongVid(vid) => {
      let ty_scheme = get_env(&cx.env, vid)?
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
          let a = Ty::Var(st.new_ty_var(false));
          let val_info = ValInfo::val(TyScheme::mono(a.clone()));
          (hashmap![vid.last.val => val_info], a, Pat::Anything)
        }
        Some(ty_scheme) => {
          let ty = instantiate(st, ty_scheme, pat.loc);
          let sym = match ty {
            Ty::Ctor(_, sym) => sym,
            _ => return Err(pat.loc.wrap(Error::PatNotConsType(ty))),
          };
          let span = get_span(&st.sym_tys, sym);
          let pat = Pat::zero(Con::Ctor(vid.last.val, span));
          (ValEnv::new(), ty, pat)
        }
      }
    }
    AstPat::Record(rows, rest_loc) => {
      if let Some(loc) = rest_loc {
        return Err(loc.wrap(Error::Todo));
      }
      let mut ve = ValEnv::new();
      let mut ty_rows = Vec::with_capacity(rows.len());
      let mut new_pats = BTreeMap::new();
      for row in rows {
        let (other_ve, ty, pat) = ck(cx, st, &row.pat)?;
        if new_pats.insert(row.lab.val, pat).is_some() {
          return Err(row.lab.loc.wrap(Error::DuplicateLabel(row.lab.val)));
        }
        env_merge(&mut ve, other_ve, row.pat.loc)?;
        ty_rows.push((row.lab.val, ty));
      }
      let new_pats: Vec<_> = new_pats.into_iter().map(|(_, pat)| pat).collect();
      let pat = Pat::record(new_pats);
      (ve, Ty::Record(ty_rows), pat)
    }
    AstPat::Tuple(pats) => {
      let mut ve = ValEnv::new();
      let mut ty_rows = Vec::with_capacity(pats.len());
      let mut new_pats = Vec::with_capacity(pats.len());
      for (idx, pat) in pats.iter().enumerate() {
        let (other_ve, ty, new_pat) = ck(cx, st, pat)?;
        env_merge(&mut ve, other_ve, pat.loc)?;
        ty_rows.push((tuple_lab(idx), ty));
        new_pats.push(new_pat);
      }
      let pat = Pat::record(new_pats);
      (ve, Ty::Record(ty_rows), pat)
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
      let pat = new_pats.into_iter().rev().fold(
        Pat::zero(Con::Ctor(StrRef::NIL, Span::Finite(2))),
        |ac, x| {
          Pat::Con(
            Con::Ctor(StrRef::CONS, Span::Finite(2)),
            vec![Pat::record(vec![x, ac])],
          )
        },
      );
      (ve, Ty::list(elem), pat)
    }
    AstPat::Ctor(long, arg) => {
      let (val_env, arg_ty, arg_pat) = ck(cx, st, arg)?;
      let (ty, pat) = ctor(cx, st, pat.loc, long, arg_ty, arg_pat)?;
      (val_env, ty, pat)
    }
    AstPat::InfixCtor(lhs, vid, rhs) => {
      let (mut val_env, lhs_ty, lhs_pat) = ck(cx, st, lhs)?;
      let (other_ve, rhs_ty, rhs_pat) = ck(cx, st, rhs)?;
      env_merge(&mut val_env, other_ve, pat.loc)?;
      let arg_ty = Ty::Record(vec![(Label::Num(1), lhs_ty), (Label::Num(2), rhs_ty)]);
      let arg_pat = Pat::record(vec![lhs_pat, rhs_pat]);
      let long = Long {
        structures: vec![],
        last: *vid,
      };
      let (ty, pat) = ctor(cx, st, pat.loc, &long, arg_ty, arg_pat)?;
      (val_env, ty, pat)
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
        return Err(vid.loc.wrap(Error::NonVarInAs(vid.val)));
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

fn ctor(
  cx: &Cx,
  st: &mut State,
  loc: Loc,
  long: &Long<StrRef>,
  arg_ty: Ty,
  arg_pat: Pat,
) -> Result<(Ty, Pat)> {
  let val_info = get_val_info(get_env(&cx.env, long)?, long.last)?;
  if val_info.id_status.is_val() {
    return Err(long.loc().wrap(Error::PatWrongIdStatus));
  }
  let (ctor_arg_ty, mut ctor_res_ty) = match instantiate(st, &val_info.ty_scheme, loc) {
    Ty::Arrow(x, y) => (*x, *y),
    ty => return Err(loc.wrap(Error::PatNotArrowType(ty))),
  };
  st.subst.unify(loc, ctor_arg_ty, arg_ty)?;
  ctor_res_ty.apply(&st.subst);
  let sym = match ctor_res_ty {
    Ty::Ctor(_, sym) => sym,
    _ => unreachable!(),
  };
  let span = get_span(&st.sym_tys, sym);
  let pat = Pat::Con(Con::Ctor(long.last.val, span), vec![arg_pat]);
  Ok((ctor_res_ty, pat))
}

fn get_span(sts: &SymTys, sym: Sym) -> Span {
  if sym == Sym::base(StrRef::EXN) {
    Span::PosInf
  } else {
    Span::Finite(sts.get(&sym).unwrap().val_env.len())
  }
}
