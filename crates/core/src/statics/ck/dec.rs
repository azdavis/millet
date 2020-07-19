//! Check declarations and expressions.

use crate::ast::{Cases, DatBind, Dec, ExBindInner, Exp, Label, Long, TyBind};
use crate::intern::StrRef;
use crate::loc::Located;
use crate::statics::ck::util::{
  env_ins, env_merge, generalize, get_env, get_ty_sym, get_val_info, insert_ty_vars, instantiate,
};
use crate::statics::ck::{exhaustive, pat, ty};
use crate::statics::types::{
  Cx, Env, Error, Item, Pat, Result, State, StrEnv, Ty, TyEnv, TyInfo, TyScheme, TyVar, Tys,
  ValEnv, ValInfo,
};
use maplit::btreemap;
use std::collections::{BTreeMap, HashMap, HashSet};

fn ck_exp(cx: &Cx, st: &mut State, exp: &Located<Exp<StrRef>>) -> Result<Ty> {
  // The special constants are as per SML Definition (1). Note that SML Definition (5) is handled by
  // the parser and SML Definition (7) is handled by having atomic and non-atomic expressions be
  // part of the same enum.
  match &exp.val {
    Exp::DecInt(_) | Exp::HexInt(_) => Ok(Ty::INT),
    Exp::DecWord(_) | Exp::HexWord(_) => Ok(Ty::WORD),
    Exp::Real(_) => Ok(Ty::REAL),
    Exp::String(_) => Ok(Ty::STRING),
    Exp::Char(_) => Ok(Ty::CHAR),
    // SML Definition (2). Note that Subst, instantiate, generalize, unify, etc are all borne from
    // the comment on this rule: "The instantiation of type schemes allows different occurrences of
    // a single longvid to assume different types."
    Exp::LongVid(vid) => {
      let val_info = get_val_info(get_env(&cx.env, vid)?, vid.last)?;
      Ok(instantiate(st, &val_info.ty_scheme))
    }
    // SML Definition (3)
    Exp::Record(rows) => {
      let mut ty_rows = BTreeMap::new();
      // SML Definition (6)
      for row in rows {
        let ty = ck_exp(cx, st, &row.val)?;
        if ty_rows.insert(row.lab.val, ty).is_some() {
          return Err(row.lab.loc.wrap(Error::DuplicateLabel(row.lab.val)));
        }
      }
      Ok(Ty::Record(ty_rows))
    }
    Exp::Select(..) => Err(exp.loc.wrap(Error::Todo("record selectors"))),
    // SML Definition Appendix A - tuples are sugar for records
    Exp::Tuple(exps) => {
      let mut ty_rows = BTreeMap::new();
      for (idx, exp) in exps.iter().enumerate() {
        let ty = ck_exp(cx, st, exp)?;
        assert!(ty_rows.insert(Label::tuple(idx), ty).is_none());
      }
      Ok(Ty::Record(ty_rows))
    }
    // SML Definition Appendix A - lists are sugar for cons + nil
    Exp::List(exps) => {
      let elem = Ty::Var(st.new_ty_var(false));
      for exp in exps {
        let ty = ck_exp(cx, st, exp)?;
        st.unify(exp.loc, elem.clone(), ty)?;
      }
      Ok(Ty::list(elem))
    }
    // SML Definition Appendix A - sequences ignore all but the last expression
    Exp::Sequence(exps) => {
      let mut ret = None;
      for exp in exps {
        ret = Some(ck_exp(cx, st, exp)?);
      }
      Ok(ret.unwrap())
    }
    // SML Definition (4)
    Exp::Let(dec, exps) => {
      let env = ck(cx, st, dec)?;
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
        return Err(loc.wrap(Error::TyNameEscape));
      }
      Ok(ty)
    }
    // SML Definition (8)
    Exp::App(func, arg) => {
      let func_ty = ck_exp(cx, st, func)?;
      let arg_ty = ck_exp(cx, st, arg)?;
      // we don't actually _need_ to case on func_ty, since the Var case is actually correct for
      // _all_ types. we just do this to produce better error messages in the Record and Ctor cases.
      match func_ty {
        Ty::Var(tv) => {
          if st.subst.is_bound(&tv) {
            Err(exp.loc.wrap(Error::NotArrowTy(func_ty)))
          } else {
            let ret_ty = Ty::Var(st.new_ty_var(false));
            let arrow_ty = Ty::Arrow(arg_ty.into(), ret_ty.clone().into());
            st.unify(exp.loc, func_ty, arrow_ty)?;
            Ok(ret_ty)
          }
        }
        Ty::Arrow(func_arg_ty, func_ret_ty) => {
          st.unify(exp.loc, *func_arg_ty, arg_ty)?;
          Ok(*func_ret_ty)
        }
        Ty::Record(_) | Ty::Ctor(_, _) => Err(exp.loc.wrap(Error::NotArrowTy(func_ty))),
      }
    }
    // SML Definition (8). Infix application is the same as `op`ing the infix operator and applying
    // it to a tuple (lhs, rhs).
    Exp::InfixApp(lhs, func, rhs) => {
      let val_info = get_val_info(&cx.env, *func)?;
      let func_ty = instantiate(st, &val_info.ty_scheme);
      let lhs_ty = ck_exp(cx, st, lhs)?;
      let rhs_ty = ck_exp(cx, st, rhs)?;
      let ret_ty = Ty::Var(st.new_ty_var(false));
      let arrow_ty = Ty::Arrow(Ty::pair(lhs_ty, rhs_ty).into(), ret_ty.clone().into());
      st.unify(exp.loc, func_ty, arrow_ty)?;
      Ok(ret_ty)
    }
    // SML Definition (9)
    Exp::Typed(inner, ty) => {
      let exp_ty = ck_exp(cx, st, inner)?;
      let ty_ty = ty::ck(cx, &st.tys, ty)?;
      st.unify(exp.loc, ty_ty, exp_ty.clone())?;
      Ok(exp_ty)
    }
    // SML Definition Appendix A - boolean operators are sugar for `if`
    Exp::Andalso(lhs, rhs) | Exp::Orelse(lhs, rhs) => {
      let lhs_ty = ck_exp(cx, st, lhs)?;
      let rhs_ty = ck_exp(cx, st, rhs)?;
      st.unify(lhs.loc, Ty::BOOL, lhs_ty)?;
      st.unify(rhs.loc, Ty::BOOL, rhs_ty)?;
      Ok(Ty::BOOL)
    }
    // SML Definition (10)
    Exp::Handle(head, cases) => {
      let head_ty = ck_exp(cx, st, head)?;
      let (pats, arg_ty, res_ty) = ck_cases(cx, st, cases)?;
      exhaustive::ck_handle(pats)?;
      st.unify(exp.loc, Ty::EXN, arg_ty)?;
      st.unify(exp.loc, head_ty.clone(), res_ty)?;
      Ok(head_ty)
    }
    // SML Definition (11)
    Exp::Raise(exp) => {
      let exp_ty = ck_exp(cx, st, exp)?;
      st.unify(exp.loc, Ty::EXN, exp_ty)?;
      Ok(Ty::Var(st.new_ty_var(false)))
    }
    // SML Definition Appendix A - `if` is sugar for casing
    Exp::If(cond, then_e, else_e) => {
      let cond_ty = ck_exp(cx, st, cond)?;
      let then_ty = ck_exp(cx, st, then_e)?;
      let else_ty = ck_exp(cx, st, else_e)?;
      st.unify(cond.loc, Ty::BOOL, cond_ty)?;
      st.unify(exp.loc, then_ty.clone(), else_ty)?;
      Ok(then_ty)
    }
    Exp::While(..) => Err(exp.loc.wrap(Error::Todo("`while`"))),
    // SML Definition Appendix A - `case` is sugar for application to a `fn`
    Exp::Case(head, cases) => {
      let head_ty = ck_exp(cx, st, head)?;
      let (pats, arg_ty, res_ty) = ck_cases(cx, st, cases)?;
      exhaustive::ck_match(pats, exp.loc)?;
      st.unify(exp.loc, head_ty, arg_ty)?;
      Ok(res_ty)
    }
    // SML Definition (12)
    Exp::Fn(cases) => {
      let (pats, arg_ty, res_ty) = ck_cases(cx, st, cases)?;
      exhaustive::ck_match(pats, exp.loc)?;
      Ok(Ty::Arrow(arg_ty.into(), res_ty.into()))
    }
  }
}

/// SML Definition (13)
fn ck_cases(cx: &Cx, st: &mut State, cases: &Cases<StrRef>) -> Result<(Vec<Located<Pat>>, Ty, Ty)> {
  let arg_ty = Ty::Var(st.new_ty_var(false));
  let res_ty = Ty::Var(st.new_ty_var(false));
  let mut pats = Vec::with_capacity(cases.arms.len());
  // SML Definition (14)
  for arm in cases.arms.iter() {
    let (val_env, pat_ty, pat) = pat::ck(cx, st, &arm.pat)?;
    pats.push(arm.pat.loc.wrap(pat));
    // TODO the Definition says this should allow new free type variables to enter the Cx; does it?
    // Also, clone in loop - expensive?
    let mut cx = cx.clone();
    cx.env.val_env.extend(val_env);
    let exp_ty = ck_exp(&cx, st, &arm.exp)?;
    st.unify(arm.pat.loc, arg_ty.clone(), pat_ty)?;
    st.unify(arm.exp.loc, res_ty.clone(), exp_ty)?;
  }
  Ok((pats, arg_ty, res_ty))
}

/// Returns `Ok(())` iff `name` is not a forbidden binding name. TODO there are more of these in
/// certain situations
fn ck_binding(name: Located<StrRef>) -> Result<()> {
  let val = name.val;
  if val == StrRef::TRUE
    || val == StrRef::FALSE
    || val == StrRef::NIL
    || val == StrRef::CONS
    || val == StrRef::REF
  {
    return Err(name.loc.wrap(Error::ForbiddenBinding(name.val)));
  }
  Ok(())
}

struct FunInfo {
  args: Vec<TyVar>,
  ret: TyVar,
}

fn fun_infos_to_ve(fun_infos: &HashMap<StrRef, FunInfo>) -> ValEnv {
  fun_infos
    .iter()
    .map(|(&name, fun_info)| {
      let ty = fun_info
        .args
        .iter()
        .rev()
        .fold(Ty::Var(fun_info.ret), |ac, &tv| {
          Ty::Arrow(Ty::Var(tv).into(), ac.into())
        });
      (name, ValInfo::val(TyScheme::mono(ty)))
    })
    .collect()
}

pub fn ck(cx: &Cx, st: &mut State, dec: &Located<Dec<StrRef>>) -> Result<Env> {
  match &dec.val {
    // SML Definition (15)
    Dec::Val(ty_vars, val_binds) => {
      let mut cx_cl;
      let cx = if ty_vars.is_empty() {
        cx
      } else {
        cx_cl = cx.clone();
        insert_ty_vars(&mut cx_cl, st, ty_vars)?;
        &cx_cl
      };
      let mut val_env = ValEnv::new();
      // SML Definition (25)
      for val_bind in val_binds {
        // SML Definition (26)
        if val_bind.rec {
          return Err(dec.loc.wrap(Error::Todo("recursive val binds")));
        }
        let (other, pat_ty, pat) = pat::ck(cx, st, &val_bind.pat)?;
        for &name in other.keys() {
          ck_binding(val_bind.pat.loc.wrap(name))?;
        }
        let exp_ty = ck_exp(cx, st, &val_bind.exp)?;
        st.unify(dec.loc, pat_ty.clone(), exp_ty)?;
        exhaustive::ck_bind(pat, val_bind.pat.loc)?;
        for (name, mut val_info) in other {
          generalize(cx, st, ty_vars, &mut val_info.ty_scheme);
          let name = val_bind.pat.loc.wrap(name);
          env_ins(&mut val_env, name, val_info, Item::Val)?;
        }
      }
      Ok(val_env.into())
    }
    // SML Definition Appendix A - `fun` is sugar for `val rec` and `case`
    Dec::Fun(ty_vars, fval_binds) => {
      let mut cx_cl;
      let cx = if ty_vars.is_empty() {
        cx
      } else {
        cx_cl = cx.clone();
        insert_ty_vars(&mut cx_cl, st, ty_vars)?;
        &cx_cl
      };
      let mut fun_infos = HashMap::with_capacity(fval_binds.len());
      for fval_bind in fval_binds {
        let first = fval_bind.cases.first().unwrap();
        let info = FunInfo {
          args: first.pats.iter().map(|_| st.new_ty_var(false)).collect(),
          ret: st.new_ty_var(false),
        };
        // copied from env_ins in util
        if fun_infos.insert(first.vid.val, info).is_some() {
          let err = Error::Duplicate(Item::Val, first.vid.val);
          return Err(first.vid.loc.wrap(err));
        }
      }
      for fval_bind in fval_binds {
        let name = fval_bind.cases.first().unwrap().vid.val;
        let info = fun_infos.get(&name).unwrap();
        let mut arg_pats = Vec::with_capacity(fval_bind.cases.len());
        for case in fval_bind.cases.iter() {
          if name != case.vid.val {
            let err = Error::FunDecNameMismatch(name, case.vid.val);
            return Err(case.vid.loc.wrap(err));
          }
          if info.args.len() != case.pats.len() {
            let err = Error::FunDecWrongNumPats(info.args.len(), case.pats.len());
            let begin = case.pats.first().unwrap().loc;
            let end = case.pats.last().unwrap().loc;
            return Err(begin.span(end).wrap(err));
          }
          let mut pats_val_env = ValEnv::new();
          let mut arg_pat = Vec::with_capacity(info.args.len());
          for (pat, &tv) in case.pats.iter().zip(info.args.iter()) {
            let (ve, pat_ty, new_pat) = pat::ck(cx, st, pat)?;
            st.unify(pat.loc, Ty::Var(tv), pat_ty)?;
            env_merge(&mut pats_val_env, ve, pat.loc, Item::Val)?;
            arg_pat.push(new_pat);
          }
          let begin = case.pats.first().unwrap().loc;
          let end = case.pats.last().unwrap().loc;
          arg_pats.push(begin.span(end).wrap(Pat::record(arg_pat)));
          if let Some(ty) = &case.ret_ty {
            let new_ty = ty::ck(cx, &st.tys, ty)?;
            st.unify(ty.loc, Ty::Var(info.ret), new_ty)?;
          }
          let mut cx = cx.clone();
          // no dupe checking here - intentionally shadow.
          cx.env.val_env.extend(fun_infos_to_ve(&fun_infos));
          cx.env.val_env.extend(pats_val_env);
          let body_ty = ck_exp(&cx, st, &case.body)?;
          st.unify(case.body.loc, Ty::Var(info.ret), body_ty)?;
        }
        let begin = fval_bind.cases.first().unwrap().vid.loc;
        let end = fval_bind.cases.last().unwrap().body.loc;
        exhaustive::ck_match(arg_pats, begin.span(end))?;
      }
      let mut val_env = fun_infos_to_ve(&fun_infos);
      for val_info in val_env.values_mut() {
        generalize(cx, st, ty_vars, &mut val_info.ty_scheme);
      }
      Ok(val_env.into())
    }
    // SML Definition (16)
    Dec::Type(ty_binds) => ck_ty_binds(cx, st, ty_binds),
    // SML Definition (17)
    Dec::Datatype(dat_binds, ty_binds) => {
      let mut env = ck_dat_binds(cx.clone(), st, dat_binds)?;
      // SML Definition Appendix A - `datatype withtype` is sugar for `datatype; type`
      let mut cx = cx.clone();
      cx.o_plus(env.clone());
      env.extend(ck_ty_binds(&cx, st, ty_binds)?);
      Ok(env)
    }
    // SML Definition (18)
    Dec::DatatypeCopy(ty_con, long) => ck_dat_copy(cx, &st.tys, *ty_con, long),
    // SML Definition (19)
    Dec::Abstype(..) => Err(dec.loc.wrap(Error::Todo("`abstype`"))),
    // SML Definition (20)
    Dec::Exception(ex_binds) => {
      let mut val_env = ValEnv::new();
      for ex_bind in ex_binds {
        let val_info = match &ex_bind.inner {
          // SML Definition (30)
          ExBindInner::Ty(ty) => match ty {
            None => ValInfo::exn(),
            Some(ty) => ValInfo::exn_fn(ty::ck(cx, &st.tys, ty)?),
          },
          // SML Definition (31)
          ExBindInner::Long(vid) => {
            let val_info = get_val_info(get_env(&cx.env, vid)?, vid.last)?;
            if !val_info.id_status.is_exn() {
              let err = Error::ExnWrongIdStatus(val_info.id_status);
              return Err(vid.loc().wrap(err));
            }
            val_info.clone()
          }
        };
        env_ins(&mut val_env, ex_bind.vid, val_info, Item::Val)?;
      }
      Ok(val_env.into())
    }
    // SML Definition (21)
    Dec::Local(fst, snd) => {
      let fst_env = ck(cx, st, fst)?;
      let mut cx = cx.clone();
      cx.o_plus(fst_env);
      ck(&cx, st, snd)
    }
    // SML Definition (22)
    Dec::Open(longs) => {
      let mut env = Env::default();
      for long in longs {
        env.extend(get_env(&cx.env, long)?.clone());
      }
      Ok(env)
    }
    // SML Definition (23), SML Definition (24)
    Dec::Seq(decs) => {
      // TODO clone in loop - expensive?
      let mut cx = cx.clone();
      let mut ret = Env::default();
      for dec in decs {
        cx.o_plus(ret.clone());
        ret.extend(ck(&cx, st, dec)?);
      }
      Ok(ret)
    }
    Dec::Infix(..) | Dec::Infixr(..) | Dec::Nonfix(..) => Ok(Env::default()),
  }
}

/// SML Definition (16)
fn ck_ty_binds(cx: &Cx, st: &mut State, ty_binds: &[TyBind<StrRef>]) -> Result<Env> {
  let mut ty_env = TyEnv::default();
  // SML Definition (27)
  for ty_bind in ty_binds {
    let mut cx_cl;
    let cx = if ty_bind.ty_vars.is_empty() {
      cx
    } else {
      cx_cl = cx.clone();
      insert_ty_vars(&mut cx_cl, st, &ty_bind.ty_vars)?;
      &cx_cl
    };
    let ty = ty::ck(cx, &st.tys, &ty_bind.ty)?;
    let sym = st.new_sym(ty_bind.ty_con);
    env_ins(&mut ty_env.inner, ty_bind.ty_con, sym, Item::Ty)?;
    let info = TyInfo {
      ty_fcn: TyScheme {
        ty_vars: ty_bind
          .ty_vars
          .iter()
          .map(|tv| {
            let tv = *cx.ty_vars.get(&tv.val).unwrap();
            st.subst.remove_bound(&tv);
            tv
          })
          .collect(),
        ty,
        overload: None,
      },
      val_env: ValEnv::new(),
      // TODO
      equality: false,
    };
    st.tys.insert(sym, info);
  }
  Ok(ty_env.into())
}

/// SML Definition (17), SML Definition (71). The checking for {datatype, constructor} {bindings,
/// descriptions} appear to be essentially identical, so we can unite the ASTs and static checking
/// functions (i.e. this function).
pub fn ck_dat_binds(mut cx: Cx, st: &mut State, dat_binds: &[DatBind<StrRef>]) -> Result<Env> {
  // these two are across all `DatBind`s.
  let mut ty_env = TyEnv::default();
  let mut val_env = ValEnv::new();
  // we must first generate new symbols for _all_ the types being defined, since they are allowed to
  // reference each other. (apparently? according to SML NJ, but it seems like the Definition does
  // not indicate this, according to my reading of e.g. SML Definition (28).)
  let mut syms = Vec::new();
  for dat_bind in dat_binds {
    // create a new symbol for the type being generated with this `DatBind`.
    let sym = st.new_sym(dat_bind.ty_con);
    // tell the original context as well as the overall `TyEnv` that we return that this new
    // datatype does exist, but tell the State that it has just an empty `ValEnv`. also perform dupe
    // checking on the name of the new type and assert for sanity checking after the dupe check.
    env_ins(&mut ty_env.inner, dat_bind.ty_con, sym, Item::Ty)?;
    cx.env.ty_env.inner.insert(dat_bind.ty_con.val, sym);
    // no assert is_none since we may be shadowing something from an earlier Dec in this Cx.
    cx.ty_names.insert(sym);
    // no mapping from ast ty vars to statics ty vars here. we just need some ty vars to make the
    // `TyScheme`. pretty much copied from insert_ty_vars. TODO DRY? this is basically the guts of
    // `insert_ty_vars`.
    let mut set = HashSet::new();
    let mut ty_vars = Vec::new();
    for tv in dat_bind.ty_vars.iter() {
      if !set.insert(tv.val.name) {
        return Err(tv.loc.wrap(Error::Duplicate(Item::TyVar, tv.val.name)));
      }
      let new_tv = st.new_ty_var(tv.val.equality);
      ty_vars.push(new_tv);
      // no need to `insert_bound` because no unifying occurs.
    }
    let ty_args: Vec<_> = ty_vars.iter().copied().map(Ty::Var).collect();
    let ty_fcn = TyScheme {
      ty_vars,
      ty: Ty::Ctor(ty_args, sym),
      overload: None,
    };
    st.tys.insert_datatype(sym, ty_fcn);
    syms.push(sym);
  }
  // SML Definition (28), SML Definition (81)
  for (dat_bind, sym) in dat_binds.iter().zip(syms) {
    // note that we have to `get` here and then `get_mut` again later because of the borrow checker.
    let info = st.tys.get(&sym);
    let mut cx_cl;
    let cx = if dat_bind.ty_vars.is_empty() {
      &cx
    } else {
      // it is here that we introduce the mapping from ast ty vars to statics ty vars. we need to do
      // that in order to check the `ConBind`s. but we cannot introduce the mapping earlier, when we
      // were generating the statics ty vars and the `Sym`s, because there may be multiple
      // identically-named ty vars in different `DatBind`s.
      //
      // if we wanted we could generate new statics type variables here, but then we'd have to use
      // those new type variables in the return type of the ctor. it shouldn't matter whether we
      // generate new type variables here or not (as mentioned, we choose to not) because both the
      // type function and the ctors of the type will each have a `TyScheme` that binds the type
      // variables appropriately, so by the magic of alpha conversion they're all distinct anyway.
      cx_cl = cx.clone();
      assert_eq!(dat_bind.ty_vars.len(), info.ty_fcn.ty_vars.len());
      for (ast_tv, &tv) in dat_bind.ty_vars.iter().zip(info.ty_fcn.ty_vars.iter()) {
        cx_cl.ty_vars.insert(ast_tv.val, tv);
      }
      &cx_cl
    };
    // this ValEnv is specific to this `DatBind`.
    let mut bind_val_env = ValEnv::new();
    let mut equality = true;
    // SML Definition (29), SML Definition (82)
    for con_bind in dat_bind.cons.iter() {
      ck_binding(con_bind.vid)?;
      // if there is no `of t`, then the type of the ctor is just `T`, where `T` is the new sym type
      // that is being defined.
      let mut ty = info.ty_fcn.ty.clone();
      if let Some(arg_ty) = &con_bind.ty {
        // if there is an `of t`, then the type of the ctor is `t -> T`. we must also update whether
        // `T` respects equality based on whether `t` does. TODO this doesn't handle the equality
        // check correctly.
        let t = ty::ck(&cx, &st.tys, arg_ty)?;
        equality = equality && t.is_equality(&st.tys);
        ty = Ty::Arrow(t.into(), ty.into());
      }
      let val_info = ValInfo::ctor(TyScheme {
        ty_vars: info.ty_fcn.ty_vars.clone(),
        ty,
        overload: None,
      });
      // insert the `ValInfo` into the _overall_ `ValEnv` with dupe checking.
      env_ins(&mut val_env, con_bind.vid, val_info.clone(), Item::Val)?;
      // _also_ insert the `ValInfo` into the `DatBind`-specific `ValEnv`, but this time dupe
      // checking is unnecessary (just assert as a sanity check).
      assert!(bind_val_env.insert(con_bind.vid.val, val_info).is_none());
    }
    // now the `ValEnv` is complete, so we may update `st.tys` with the true definition of this
    // datatype. TODO closure?
    st.tys.finish_datatype(&sym, bind_val_env, equality);
  }
  Ok(Env {
    ty_env,
    val_env,
    str_env: StrEnv::new(),
  })
}

/// SML Definition (18), SML Definition (72)
pub fn ck_dat_copy(
  cx: &Cx,
  tys: &Tys,
  ty_con: Located<StrRef>,
  long: &Long<StrRef>,
) -> Result<Env> {
  let sym = get_ty_sym(get_env(&cx.env, long)?, long.last)?;
  let info = tys.get(&sym);
  if info.val_env.is_empty() {
    return Err(long.loc().wrap(Error::DatatypeCopyNotDatatype));
  }
  Ok(Env {
    str_env: StrEnv::new(),
    ty_env: TyEnv {
      inner: btreemap![ty_con.val => sym],
    },
    val_env: info.val_env.clone(),
  })
}
