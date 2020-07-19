//! Check patterns.

use crate::ast::{Label, Long, Pat as AstPat};
use crate::intern::StrRef;
use crate::loc::{Loc, Located};
use crate::statics::ck::ty;
use crate::statics::ck::util::{env_ins, env_merge, get_env, get_val_info, instantiate};
use crate::statics::types::{
  Con, Cx, Error, Item, Pat, Result, Span, State, Sym, Ty, TyScheme, Tys, ValEnv, ValInfo,
};
use maplit::btreemap;
use std::collections::BTreeMap;

pub fn ck(cx: &Cx, st: &mut State, pat: &Located<AstPat<StrRef>>) -> Result<(ValEnv, Ty, Pat)> {
  // Wildcard is by SML Definition (32), special constants are by SML Definition (33). Additionally,
  // SML Definition (37) is handled by the parser, and SML Definition (40) is handed because atomic
  // and non-atomic Pats are both in the same enum.
  match &pat.val {
    AstPat::Wildcard => Ok((ValEnv::new(), Ty::Var(st.new_ty_var(false)), Pat::Anything)),
    AstPat::DecInt(n) | AstPat::HexInt(n) => Ok((ValEnv::new(), Ty::INT, Pat::zero(Con::Int(*n)))),
    AstPat::DecWord(n) | AstPat::HexWord(n) => {
      Ok((ValEnv::new(), Ty::WORD, Pat::zero(Con::Word(*n))))
    }
    AstPat::String(s) => Ok((ValEnv::new(), Ty::STRING, Pat::zero(Con::String(*s)))),
    AstPat::Char(c) => Ok((ValEnv::new(), Ty::CHAR, Pat::zero(Con::Char(*c)))),
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
        // SML Definition (34)
        None => {
          let a = Ty::Var(st.new_ty_var(false));
          let val_info = ValInfo::val(TyScheme::mono(a.clone()));
          Ok((btreemap![vid.last.val => val_info], a, Pat::Anything))
        }
        // SML Definition (35)
        Some(ty_scheme) => {
          let ty = instantiate(st, ty_scheme);
          let sym = match ty {
            Ty::Ctor(_, sym) => sym,
            _ => return Err(pat.loc.wrap(Error::PatNotConsTy(ty))),
          };
          let span = get_span(&st.tys, sym);
          let pat = Pat::zero(Con::Ctor(vid.last.val, span));
          Ok((ValEnv::new(), ty, pat))
        }
      }
    }
    // SML Definition (36)
    AstPat::Record(rows, rest_loc) => {
      // SML Definition (38)
      if let Some(loc) = rest_loc {
        return Err(loc.wrap(Error::Todo("rest patterns")));
      }
      let mut val_env = ValEnv::new();
      let mut ty_rows = BTreeMap::new();
      let mut new_pats = BTreeMap::new();
      // SML Definition (39)
      for row in rows {
        let (other_ve, ty, pat) = ck(cx, st, &row.val)?;
        if new_pats.insert(row.lab.val, pat).is_some() {
          return Err(row.lab.loc.wrap(Error::DuplicateLabel(row.lab.val)));
        }
        env_merge(&mut val_env, other_ve, row.val.loc, Item::Val)?;
        assert!(ty_rows.insert(row.lab.val, ty).is_none());
      }
      let new_pats: Vec<_> = new_pats.into_iter().map(|(_, pat)| pat).collect();
      let pat = Pat::record(new_pats);
      Ok((val_env, Ty::Record(ty_rows), pat))
    }
    // SML Definition Appendix A - tuple patterns are sugar for records
    AstPat::Tuple(pats) => {
      let mut val_env = ValEnv::new();
      let mut ty_rows = BTreeMap::new();
      let mut new_pats = Vec::with_capacity(pats.len());
      for (idx, pat) in pats.iter().enumerate() {
        let (other_ve, ty, new_pat) = ck(cx, st, pat)?;
        env_merge(&mut val_env, other_ve, pat.loc, Item::Val)?;
        assert!(ty_rows.insert(Label::tuple(idx), ty).is_none());
        new_pats.push(new_pat);
      }
      let pat = Pat::record(new_pats);
      Ok((val_env, Ty::Record(ty_rows), pat))
    }
    // SML Definition Appendix A - list patterns are sugar for constructors
    AstPat::List(pats) => {
      let elem = Ty::Var(st.new_ty_var(false));
      let mut val_env = ValEnv::new();
      let mut new_pats = Vec::with_capacity(pats.len());
      for pat in pats {
        let (other_ve, ty, new_pat) = ck(cx, st, pat)?;
        env_merge(&mut val_env, other_ve, pat.loc, Item::Val)?;
        st.unify(pat.loc, elem.clone(), ty)?;
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
      Ok((val_env, Ty::list(elem), pat))
    }
    // SML Definition (41)
    AstPat::Ctor(long, arg) => {
      let (val_env, arg_ty, arg_pat) = ck(cx, st, arg)?;
      let (ty, pat) = ctor(cx, st, pat.loc, long, arg_ty, arg_pat)?;
      Ok((val_env, ty, pat))
    }
    // SML Definition (41). Infix ctors are the same as `op`ing the ctor and applying it to the
    // tuple (lhs, rhs).
    AstPat::InfixCtor(lhs, vid, rhs) => {
      let (mut val_env, lhs_ty, lhs_pat) = ck(cx, st, lhs)?;
      let (other_ve, rhs_ty, rhs_pat) = ck(cx, st, rhs)?;
      env_merge(&mut val_env, other_ve, pat.loc, Item::Val)?;
      let arg_ty = Ty::pair(lhs_ty, rhs_ty);
      let arg_pat = Pat::record(vec![lhs_pat, rhs_pat]);
      let long = Long {
        structures: vec![],
        last: *vid,
      };
      let (ty, pat) = ctor(cx, st, pat.loc, &long, arg_ty, arg_pat)?;
      Ok((val_env, ty, pat))
    }
    // SML Definition (42)
    AstPat::Typed(inner_pat, ty) => {
      let (val_env, pat_ty, inner_pat) = ck(cx, st, inner_pat)?;
      let ty = ty::ck(cx, &st.tys, ty)?;
      st.unify(pat.loc, ty, pat_ty.clone())?;
      Ok((val_env, pat_ty, inner_pat))
    }
    // SML Definition (43)
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
        let ty = ty::ck(cx, &st.tys, ty)?;
        st.unify(pat.loc, ty, pat_ty.clone())?;
      }
      let val_info = ValInfo::val(TyScheme::mono(pat_ty.clone()));
      env_ins(&mut val_env, *vid, val_info, Item::Val)?;
      Ok((val_env, pat_ty, inner_pat))
    }
  }
}

/// SML Definition (41)
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
  let (ctor_arg_ty, mut ctor_res_ty) = match instantiate(st, &val_info.ty_scheme) {
    Ty::Arrow(x, y) => (*x, *y),
    ty => return Err(loc.wrap(Error::PatNotArrowTy(ty))),
  };
  st.unify(loc, ctor_arg_ty, arg_ty)?;
  ctor_res_ty.apply(&st.subst);
  let sym = match ctor_res_ty {
    Ty::Ctor(_, sym) => sym,
    _ => unreachable!(),
  };
  let span = get_span(&st.tys, sym);
  let pat = Pat::Con(Con::Ctor(long.last.val, span), vec![arg_pat]);
  Ok((ctor_res_ty, pat))
}

fn get_span(tys: &Tys, sym: Sym) -> Span {
  assert!(sym != Sym::CHAR);
  assert!(sym != Sym::STRING);
  assert!(sym != Sym::WORD);
  assert!(sym != Sym::INT);
  assert!(sym != Sym::REAL);
  if sym == Sym::EXN {
    Span::PosInf
  } else {
    Span::Finite(tys.get(&sym).val_env.len())
  }
}
