//! Checking ASTs for validity.
//!
//! We pass around one big `Subst` in the `State`. This `Subst` is constantly mutably updated as we
//! discover more types that must unify. However, note that `Subst#unify` accepts the two `Ty`s by
//! move, and so does not update the types themselves if and when the call to `unify` has
//! successfully updated the `Subst`. This means that _if_ you want to have the types be updated as
//! well, you must call `Ty#apply` with the new `Subst`.
//!
//! It is only strictly necessary to call `Ty#apply` in the following situations:
//!
//! 1. Right before returning from `instantiate`. This is a bit of a special case because the
//!    `Subst` there is just a one-off which maps old type variables to new type variables, and is
//!    not part of the overall global `Subst`.
//! 2. Right before calling `exhaustive::ck`. This is so that we may correctly compute the
//!    obligations the patterns need to fulfil.
//! 3. Right before calling `generalize`. This is so that we know exactly what type variables ought
//!    to be generalized and which have already been solved to concrete types by the `Subst`.
//! 4. Right before checking for type name escape. This is so that we know exactly what type names
//!    are mentioned in the type we are checking.
//!
//! In short, we only need to call `apply` when we truly need access to _everything_ we currently
//! know about this type. In many situations, for instance, we don't need to call `apply` on a fresh
//! type variable we are about to return from `ck_exp` (e.g. the App case) because we have already
//! recorded the relevant information in the `Subst` and this information will be surfaced later by
//! one of the above noted places that we do call `apply`.

use crate::ast::{Cases, Dec, Exp, Label, Long, Pat as AstPat, StrDec, TopDec, Ty as AstTy};
use crate::intern::StrRef;
use crate::loc::{Loc, Located};
use crate::statics::exhaustive;
use crate::statics::types::{
  Basis, Cx, DatatypeInfo, Datatypes, Env, Item, Pat, Result, State, StaticsError, StrEnv, Subst,
  Ty, TyEnv, TyInfo, TyScheme, TyVarSet, ValEnv, ValInfo,
};
use maplit::hashmap;
use std::collections::{HashMap, HashSet};
use std::convert::TryInto as _;

fn instantiate(st: &mut State, ty_scheme: &TyScheme, loc: Loc) -> Ty {
  let mut subst = Subst::default();
  match &ty_scheme.overload {
    None => {
      for &tv in ty_scheme.ty_vars.iter() {
        subst.insert(tv, Ty::Var(st.new_ty_var(tv.equality)));
      }
    }
    Some(overloads) => {
      // NOTE it might be better to have TyScheme be an enum to make these illegal states impossible
      // to represent in the types. But then again, overloading is uncommon since only the standard
      // library is permitted to do it. In fact, the fact that only the standard library may do
      // overloading, and that all overloaded operators are similar in that every one of them has
      // only one overloaded type variable which is meant to be "the same" within a single
      // instantiation, leads to this slightly hacky implementation.
      let mut iter = ty_scheme.ty_vars.iter().copied();
      let tv = iter.next().unwrap();
      assert!(iter.next().is_none());
      assert!(!overloads.is_empty());
      assert!(!tv.equality);
      let new_tv = st.new_ty_var(false);
      subst.insert(tv, Ty::Var(new_tv));
      st.overload.push((loc, new_tv, overloads.clone()));
    }
  }
  let mut ty = ty_scheme.ty.clone();
  ty.apply(&subst);
  ty
}

fn generalize(ty_env: &TyEnv, dts: &Datatypes, ty_scheme: &mut TyScheme) {
  assert!(ty_scheme.ty_vars.is_empty());
  assert!(ty_scheme.overload.is_none());
  ty_scheme.ty_vars = ty_scheme
    .ty
    .free_ty_vars()
    .difference(&ty_env.free_ty_vars(dts))
    .copied()
    .collect();
}

fn get_env<'cx>(cx: &'cx Cx, long: &Long<StrRef>) -> Result<&'cx Env> {
  let mut ret = &cx.env;
  for &s in long.structures.iter() {
    ret = match ret.str_env.get(&s.val) {
      None => return Err(s.loc.wrap(StaticsError::Undefined(Item::Structure, s.val))),
      Some(x) => x,
    }
  }
  Ok(ret)
}

fn get_val_info(env: &Env, name: Located<StrRef>) -> Result<&ValInfo> {
  match env.val_env.get(&name.val) {
    None => Err(
      name
        .loc
        .wrap(StaticsError::Undefined(Item::Value, name.val)),
    ),
    Some(val_info) => Ok(val_info),
  }
}

fn tuple_lab(idx: usize) -> Label {
  Label::Num((idx + 1).try_into().unwrap())
}

fn ck_exp(cx: &Cx, st: &mut State, exp: &Located<Exp<StrRef>>) -> Result<Ty> {
  let ret = match &exp.val {
    Exp::DecInt(_) => Ty::INT,
    Exp::HexInt(_) => Ty::INT,
    Exp::DecWord(_) => Ty::WORD,
    Exp::HexWord(_) => Ty::WORD,
    Exp::Real(_) => Ty::REAL,
    Exp::String(_) => Ty::STRING,
    Exp::Char(_) => Ty::CHAR,
    Exp::LongVid(vid) => {
      let val_info = get_val_info(get_env(cx, vid)?, vid.last)?;
      instantiate(st, &val_info.ty_scheme, exp.loc)
    }
    Exp::Record(rows) => {
      let mut ty_rows = Vec::with_capacity(rows.len());
      let mut keys = HashSet::with_capacity(rows.len());
      for row in rows {
        let ty = ck_exp(cx, st, &row.exp)?;
        if !keys.insert(row.lab.val) {
          return Err(row.lab.loc.wrap(StaticsError::DuplicateLabel(row.lab.val)));
        }
        ty_rows.push((row.lab.val, ty));
      }
      Ty::Record(ty_rows)
    }
    Exp::Select(..) => return Err(exp.loc.wrap(StaticsError::Todo)),
    Exp::Tuple(exps) => {
      let mut ty_rows = Vec::with_capacity(exps.len());
      for (idx, exp) in exps.iter().enumerate() {
        let ty = ck_exp(cx, st, exp)?;
        let lab = tuple_lab(idx);
        ty_rows.push((lab, ty));
      }
      Ty::Record(ty_rows)
    }
    Exp::List(exps) => {
      let elem = Ty::Var(st.new_ty_var(false));
      for exp in exps {
        let ty = ck_exp(cx, st, exp)?;
        st.subst.unify(exp.loc, elem.clone(), ty)?;
      }
      Ty::list(elem)
    }
    Exp::Sequence(exps) => {
      let mut ret = None;
      for exp in exps {
        ret = Some(ck_exp(cx, st, exp)?);
      }
      ret.unwrap()
    }
    Exp::Let(dec, exps) => {
      let env = ck_dec(cx, st, dec)?;
      let mut cx = cx.clone();
      let ty_names = cx.ty_names.clone();
      cx.o_plus(env);
      let mut last = None;
      for exp in exps {
        last = Some((exp.loc, ck_exp(&cx, st, exp)?));
      }
      let (loc, mut ty) = last.unwrap();
      ty.apply(&st.subst);
      if !ty.ty_names().is_subset(&ty_names) {
        return Err(loc.wrap(StaticsError::TyNameEscape));
      }
      ty
    }
    Exp::App(func, arg) => {
      let func_ty = ck_exp(cx, st, func)?;
      let arg_ty = ck_exp(cx, st, arg)?;
      let ret_ty = Ty::Var(st.new_ty_var(false));
      let arrow_ty = Ty::Arrow(arg_ty.into(), ret_ty.clone().into());
      st.subst.unify(exp.loc, func_ty, arrow_ty)?;
      ret_ty
    }
    Exp::InfixApp(lhs, func, rhs) => {
      let val_info = get_val_info(&cx.env, *func)?;
      let func_ty = instantiate(st, &val_info.ty_scheme, exp.loc);
      let lhs_ty = ck_exp(cx, st, lhs)?;
      let rhs_ty = ck_exp(cx, st, rhs)?;
      let ret_ty = Ty::Var(st.new_ty_var(false));
      let arrow_ty = Ty::Arrow(
        Ty::Record(vec![(Label::Num(1), lhs_ty), (Label::Num(2), rhs_ty)]).into(),
        ret_ty.clone().into(),
      );
      st.subst.unify(exp.loc, func_ty, arrow_ty)?;
      ret_ty
    }
    Exp::Typed(inner, ty) => {
      let exp_ty = ck_exp(cx, st, inner)?;
      let ty_ty = ck_ty(cx, st, ty)?;
      st.subst.unify(exp.loc, ty_ty, exp_ty.clone())?;
      exp_ty
    }
    Exp::Andalso(lhs, rhs) | Exp::Orelse(lhs, rhs) => {
      let lhs_ty = ck_exp(cx, st, lhs)?;
      let rhs_ty = ck_exp(cx, st, rhs)?;
      st.subst.unify(lhs.loc, Ty::BOOL, lhs_ty)?;
      st.subst.unify(rhs.loc, Ty::BOOL, rhs_ty)?;
      Ty::BOOL
    }
    Exp::Handle(head, cases) => {
      let head_ty = ck_exp(cx, st, head)?;
      let (arg_ty, res_ty) = ck_cases(cx, st, cases, exp.loc)?;
      st.subst.unify(exp.loc, Ty::EXN, arg_ty)?;
      st.subst.unify(exp.loc, head_ty.clone(), res_ty)?;
      head_ty
    }
    Exp::Raise(exp) => {
      let exp_ty = ck_exp(cx, st, exp)?;
      st.subst.unify(exp.loc, Ty::EXN, exp_ty)?;
      Ty::Var(st.new_ty_var(false))
    }
    Exp::If(cond, then_e, else_e) => {
      let cond_ty = ck_exp(cx, st, cond)?;
      let then_ty = ck_exp(cx, st, then_e)?;
      let else_ty = ck_exp(cx, st, else_e)?;
      st.subst.unify(cond.loc, Ty::BOOL, cond_ty)?;
      st.subst.unify(exp.loc, then_ty.clone(), else_ty)?;
      then_ty
    }
    Exp::While(..) => return Err(exp.loc.wrap(StaticsError::Todo)),
    Exp::Case(head, cases) => {
      let head_ty = ck_exp(cx, st, head)?;
      let (arg_ty, res_ty) = ck_cases(cx, st, cases, exp.loc)?;
      st.subst.unify(exp.loc, head_ty, arg_ty)?;
      res_ty
    }
    Exp::Fn(cases) => {
      let (arg_ty, res_ty) = ck_cases(cx, st, cases, exp.loc)?;
      Ty::Arrow(arg_ty.into(), res_ty.into())
    }
  };
  Ok(ret)
}

fn ck_cases(cx: &Cx, st: &mut State, cases: &Cases<StrRef>, loc: Loc) -> Result<(Ty, Ty)> {
  let mut arg_ty = Ty::Var(st.new_ty_var(false));
  let res_ty = Ty::Var(st.new_ty_var(false));
  let mut pats = Vec::with_capacity(cases.arms.len());
  for arm in cases.arms.iter() {
    let (val_env, pat_ty, pat) = ck_pat(cx, st, &arm.pat)?;
    pats.push(arm.pat.loc.wrap(pat));
    // TODO what about type variables? The Definition says this should allow new free type variables
    // to enter the Cx, but right now we do nothing with `cx.ty_vars`. TODO clone in loop -
    // expensive?
    let mut cx = cx.clone();
    cx.env.val_env.extend(val_env);
    let exp_ty = ck_exp(&cx, st, &arm.exp)?;
    st.subst.unify(arm.pat.loc, arg_ty.clone(), pat_ty)?;
    st.subst.unify(arm.exp.loc, res_ty.clone(), exp_ty)?;
  }
  arg_ty.apply(&st.subst);
  if exhaustive::ck(&st.datatypes, &arg_ty, pats)? {
    Ok((arg_ty, res_ty))
  } else {
    Err(loc.wrap(StaticsError::NonExhaustiveMatch))
  }
}

fn ck_ty(cx: &Cx, st: &mut State, ty: &Located<AstTy<StrRef>>) -> Result<Ty> {
  let ret = match &ty.val {
    AstTy::TyVar(_) => {
      //
      return Err(ty.loc.wrap(StaticsError::Todo));
    }
    AstTy::Record(rows) => {
      let mut ty_rows = Vec::with_capacity(rows.len());
      let mut keys = HashSet::with_capacity(rows.len());
      for row in rows {
        let ty = ck_ty(cx, st, &row.ty)?;
        if !keys.insert(row.lab.val) {
          return Err(row.lab.loc.wrap(StaticsError::DuplicateLabel(row.lab.val)));
        }
        ty_rows.push((row.lab.val, ty));
      }
      Ty::Record(ty_rows)
    }
    AstTy::Tuple(tys) => {
      let mut ty_rows = Vec::with_capacity(tys.len());
      for (idx, ty) in tys.iter().enumerate() {
        let ty = ck_ty(cx, st, ty)?;
        let lab = tuple_lab(idx);
        ty_rows.push((lab, ty));
      }
      Ty::Record(ty_rows)
    }
    AstTy::TyCon(args, name) => {
      let env = get_env(cx, name)?;
      let ty_fcn = match env.ty_env.inner.get(&name.last.val) {
        None => {
          return Err(
            name
              .last
              .loc
              .wrap(StaticsError::Undefined(Item::Type, name.last.val)),
          )
        }
        // NOTE could avoid this clone if we separated datatypes from State
        Some(x) => x.ty_fcn(&st.datatypes).clone(),
      };
      if ty_fcn.ty_vars.len() != args.len() {
        let err = StaticsError::WrongNumTyArgs(ty_fcn.ty_vars.len(), args.len());
        return Err(ty.loc.wrap(err));
      }
      let mut new_args = Vec::with_capacity(ty_fcn.ty_vars.len());
      for ty in args {
        new_args.push(ck_ty(cx, st, ty)?);
      }
      ty_fcn.apply_args(new_args)
    }
    AstTy::Arrow(arg, res) => {
      let arg = ck_ty(cx, st, arg)?;
      let res = ck_ty(cx, st, res)?;
      Ty::Arrow(arg.into(), res.into())
    }
  };
  Ok(ret)
}

fn ck_binding(name: Located<StrRef>) -> Result<()> {
  for &other in [
    StrRef::TRUE,
    StrRef::FALSE,
    StrRef::NIL,
    StrRef::CONS,
    StrRef::REF,
  ]
  .iter()
  {
    if name.val == other {
      return Err(name.loc.wrap(StaticsError::ForbiddenBinding(name.val)));
    }
  }
  Ok(())
}

fn ck_dec(cx: &Cx, st: &mut State, dec: &Located<Dec<StrRef>>) -> Result<Env> {
  let ret = match &dec.val {
    Dec::Val(ty_vars, val_binds) => {
      if let Some(tv) = ty_vars.first() {
        return Err(tv.loc.wrap(StaticsError::Todo));
      }
      let mut val_env = ValEnv::new();
      for val_bind in val_binds {
        if val_bind.rec {
          return Err(dec.loc.wrap(StaticsError::Todo));
        }
        let (other, mut pat_ty, pat) = ck_pat(cx, st, &val_bind.pat)?;
        for &name in other.keys() {
          ck_binding(val_bind.pat.loc.wrap(name))?;
        }
        let exp_ty = ck_exp(cx, st, &val_bind.exp)?;
        st.subst.unify(dec.loc, pat_ty.clone(), exp_ty)?;
        pat_ty.apply(&st.subst);
        if !exhaustive::ck(&st.datatypes, &pat_ty, vec![val_bind.pat.loc.wrap(pat)])? {
          return Err(val_bind.pat.loc.wrap(StaticsError::NonExhaustiveBinding));
        }
        for (name, mut val_info) in other {
          // NOTE could avoid this assert by having ck_pat return not a ValEnv but HashMap<StrRef,
          // (Ty, IdStatus)>. but this assert should hold because we the only TySchemes we put into
          // the ValEnv returned from ck_pat are mono.
          assert!(val_info.ty_scheme.ty_vars.is_empty());
          val_info.ty_scheme.ty.apply(&st.subst);
          generalize(&cx.env.ty_env, &st.datatypes, &mut val_info.ty_scheme);
          env_ins(&mut val_env, val_bind.pat.loc.wrap(name), val_info)?;
        }
      }
      val_env.into()
    }
    Dec::Fun(_, _) => {
      //
      return Err(dec.loc.wrap(StaticsError::Todo));
    }
    Dec::Type(ty_binds) => {
      let mut ty_env = TyEnv::default();
      for ty_bind in ty_binds {
        if !ty_bind.ty_vars.is_empty() {
          return Err(dec.loc.wrap(StaticsError::Todo));
        }
        let ty = ck_ty(cx, st, &ty_bind.ty)?;
        let info = TyInfo::Alias(TyScheme::mono(ty));
        if ty_env.inner.insert(ty_bind.ty_con.val, info).is_some() {
          return Err(
            ty_bind
              .ty_con
              .loc
              .wrap(StaticsError::Redefined(ty_bind.ty_con.val)),
          );
        }
      }
      ty_env.into()
    }
    Dec::Datatype(dat_binds, ty_binds) => {
      if let Some(x) = ty_binds.first() {
        return Err(x.ty_con.loc.wrap(StaticsError::Todo));
      }
      let mut cx = cx.clone();
      // these two are across all dat_binds.
      let mut ty_env = TyEnv::default();
      let mut val_env = ValEnv::new();
      for dat_bind in dat_binds {
        if let Some(x) = dat_bind.ty_vars.first() {
          return Err(x.loc.wrap(StaticsError::Todo));
        }
        // create a new symbol for the type being generated with this DatBind.
        let sym = st.new_sym(dat_bind.ty_con);
        // tell the original context as well as the overall TyEnv that we return that this new
        // datatype does exist, but tell the State that it has just an empty ValEnv. also perform
        // dupe checking on the name of the new type and assert for sanity checking after the dupe
        // check.
        env_ins(
          &mut cx.env.ty_env.inner,
          dat_bind.ty_con,
          TyInfo::Datatype(sym),
        )?;
        // no assert is_none since we may be shadowing something from an earlier Dec in this Cx.
        cx.ty_names.insert(dat_bind.ty_con.val);
        assert!(ty_env
          .inner
          .insert(dat_bind.ty_con.val, TyInfo::Datatype(sym))
          .is_none());
        assert!(st
          .datatypes
          .insert(
            sym,
            DatatypeInfo {
              ty_fcn: TyScheme::mono(Ty::Ctor(Vec::new(), sym)),
              val_env: ValEnv::new(),
            },
          )
          .is_none());
        // this ValEnv is specific to this DatBind.
        let mut bind_val_env = ValEnv::new();
        for con_bind in dat_bind.cons.iter() {
          ck_binding(con_bind.vid)?;
          // the type being defined in this declaration is `ty`.
          let mut ty = Ty::Ctor(Vec::new(), sym);
          if let Some(arg_ty) = &con_bind.ty {
            // if there is an `of t`, then the type of the ctor is `t -> ty`. otherwise, the type of
            // the ctor is just `ty`.
            ty = Ty::Arrow(ck_ty(&cx, st, arg_ty)?.into(), ty.into());
          }
          // insert the ValInfo into the _overall_ ValEnv with dupe checking.
          env_ins(
            &mut val_env,
            con_bind.vid,
            ValInfo::ctor(TyScheme::mono(ty.clone())),
          )?;
          // _also_ insert the ValInfo into the DatBind-specific ValEnv, but this time dupe checking
          // is unnecessary (just assert as a sanity check).
          assert!(bind_val_env
            .insert(con_bind.vid.val, ValInfo::ctor(TyScheme::mono(ty)))
            .is_none());
        }
        // now the ValEnv is complete, so we may update st.datatypes with the true definition of
        // this datatype. assert to check that we inserted the fake answer earlier.
        assert!(st
          .datatypes
          .insert(
            sym,
            DatatypeInfo {
              ty_fcn: TyScheme::mono(Ty::Ctor(Vec::new(), sym)),
              val_env: bind_val_env,
            },
          )
          .is_some());
      }
      Env {
        ty_env,
        val_env,
        str_env: StrEnv::new(),
      }
    }
    Dec::DatatypeCopy(_, _) => {
      //
      return Err(dec.loc.wrap(StaticsError::Todo));
    }
    Dec::Abstype(..) => return Err(dec.loc.wrap(StaticsError::Todo)),
    Dec::Exception(_) => {
      //
      return Err(dec.loc.wrap(StaticsError::Todo));
    }
    Dec::Local(_, _) => {
      //
      return Err(dec.loc.wrap(StaticsError::Todo));
    }
    Dec::Open(_) => {
      //
      return Err(dec.loc.wrap(StaticsError::Todo));
    }
    Dec::Seq(decs) => {
      // TODO clone in loop - expensive?
      let mut cx = cx.clone();
      let mut ret = Env::default();
      for dec in decs {
        cx.o_plus(ret.clone());
        ret.extend(ck_dec(&cx, st, dec)?);
      }
      ret
    }
    Dec::Infix(..) | Dec::Infixr(..) | Dec::Nonfix(..) => Env::default(),
  };
  Ok(ret)
}

fn env_ins<T>(map: &mut HashMap<StrRef, T>, key: Located<StrRef>, val: T) -> Result<()> {
  if map.insert(key.val, val).is_some() {
    Err(key.loc.wrap(StaticsError::Redefined(key.val)))
  } else {
    Ok(())
  }
}

fn env_merge<T>(lhs: &mut HashMap<StrRef, T>, rhs: HashMap<StrRef, T>, loc: Loc) -> Result<()> {
  for (key, val) in rhs {
    env_ins(lhs, loc.wrap(key), val)?;
  }
  Ok(())
}

fn ck_pat(cx: &Cx, st: &mut State, pat: &Located<AstPat<StrRef>>) -> Result<(ValEnv, Ty, Pat)> {
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
        let (other_ve, ty, pat) = ck_pat(cx, st, &row.pat)?;
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
        let (other_ve, ty, new_pat) = ck_pat(cx, st, pat)?;
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
        let (other_ve, ty, new_pat) = ck_pat(cx, st, pat)?;
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
      let (val_env, arg_ty, arg_pat) = ck_pat(cx, st, arg)?;
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
      let (mut val_env, lhs_ty, lhs_pat) = ck_pat(cx, st, lhs)?;
      let (other_ve, rhs_ty, rhs_pat) = ck_pat(cx, st, rhs)?;
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
      let (val_env, pat_ty, inner_pat) = ck_pat(cx, st, inner_pat)?;
      let ty = ck_ty(cx, st, ty)?;
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
      let (mut val_env, pat_ty, inner_pat) = ck_pat(cx, st, inner_pat)?;
      if let Some(ty) = ty {
        let ty = ck_ty(cx, st, ty)?;
        st.subst.unify(pat.loc, ty, pat_ty.clone())?;
      }
      let val_info = ValInfo::val(TyScheme::mono(pat_ty.clone()));
      env_ins(&mut val_env, *vid, val_info)?;
      (val_env, pat_ty, inner_pat)
    }
  };
  Ok(ret)
}

pub fn ck_top_dec(bs: Basis, st: &mut State, top_dec: &Located<TopDec<StrRef>>) -> Result<Basis> {
  match &top_dec.val {
    TopDec::StrDec(str_dec) => match &str_dec.val {
      StrDec::Dec(dec) => {
        let mut cx = Cx {
          ty_names: bs.ty_names,
          ty_vars: TyVarSet::new(),
          env: bs.env,
        };
        cx.o_plus(ck_dec(&cx, st, dec)?);
        Ok(Basis {
          env: cx.env,
          ty_names: cx.ty_names,
          ..bs
        })
      }
      StrDec::Structure(_) => Err(top_dec.loc.wrap(StaticsError::Todo)),
      StrDec::Local(_, _) => Err(top_dec.loc.wrap(StaticsError::Todo)),
      StrDec::Seq(_) => Err(top_dec.loc.wrap(StaticsError::Todo)),
    },
    TopDec::SigDec(_) => Err(top_dec.loc.wrap(StaticsError::Todo)),
    TopDec::FunDec(_) => Err(top_dec.loc.wrap(StaticsError::Todo)),
  }
}
