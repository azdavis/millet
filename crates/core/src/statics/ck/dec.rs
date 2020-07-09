//! Check declarations and expressions.

use crate::ast::{Cases, DatBind, Dec, ExBindInner, Exp, Label, Long};
use crate::intern::StrRef;
use crate::loc::{Loc, Located};
use crate::statics::ck::util::{
  env_ins, env_merge, generalize, get_env, get_ty_info, get_val_info, instantiate, tuple_lab,
};
use crate::statics::ck::{exhaustive, pat, ty};
use crate::statics::types::{
  Cx, Env, Error, Pat, Result, State, StrEnv, SymTyInfo, Ty, TyEnv, TyInfo, TyScheme, TyVar,
  ValEnv, ValInfo,
};
use maplit::hashmap;
use std::collections::{HashMap, HashSet};

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
      let val_info = get_val_info(get_env(&cx.env, vid)?, vid.last)?;
      instantiate(st, &val_info.ty_scheme, exp.loc)
    }
    Exp::Record(rows) => {
      let mut ty_rows = Vec::with_capacity(rows.len());
      let mut keys = HashSet::with_capacity(rows.len());
      for row in rows {
        let ty = ck_exp(cx, st, &row.exp)?;
        if !keys.insert(row.lab.val) {
          return Err(row.lab.loc.wrap(Error::DuplicateLabel(row.lab.val)));
        }
        ty_rows.push((row.lab.val, ty));
      }
      Ty::Record(ty_rows)
    }
    Exp::Select(..) => return Err(exp.loc.wrap(Error::Todo)),
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
      let ty_ty = ty::ck(cx, st, ty)?;
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
    Exp::While(..) => return Err(exp.loc.wrap(Error::Todo)),
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
  let arg_ty = Ty::Var(st.new_ty_var(false));
  let res_ty = Ty::Var(st.new_ty_var(false));
  let mut pats = Vec::with_capacity(cases.arms.len());
  for arm in cases.arms.iter() {
    let (val_env, pat_ty, pat) = pat::ck(cx, st, &arm.pat)?;
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
  exhaustive::ck_match(pats, loc)?;
  Ok((arg_ty, res_ty))
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
      return Err(name.loc.wrap(Error::ForbiddenBinding(name.val)));
    }
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
  let ret = match &dec.val {
    Dec::Val(ty_vars, val_binds) => {
      if let Some(tv) = ty_vars.first() {
        return Err(tv.loc.wrap(Error::Todo));
      }
      let mut val_env = ValEnv::new();
      for val_bind in val_binds {
        if val_bind.rec {
          return Err(dec.loc.wrap(Error::Todo));
        }
        let (other, pat_ty, pat) = pat::ck(cx, st, &val_bind.pat)?;
        for &name in other.keys() {
          ck_binding(val_bind.pat.loc.wrap(name))?;
        }
        let exp_ty = ck_exp(cx, st, &val_bind.exp)?;
        st.subst.unify(dec.loc, pat_ty.clone(), exp_ty)?;
        exhaustive::ck_bind(pat, val_bind.pat.loc)?;
        for (name, mut val_info) in other {
          // NOTE could avoid this assert by having ck_pat return not a ValEnv but HashMap<StrRef,
          // (Ty, IdStatus)>. but this assert should hold because we the only TySchemes we put into
          // the ValEnv returned from ck_pat are mono.
          assert!(val_info.ty_scheme.ty_vars.is_empty());
          val_info.ty_scheme.ty.apply(&st.subst);
          generalize(&cx.env.ty_env, &st.sym_tys, &mut val_info.ty_scheme);
          env_ins(&mut val_env, val_bind.pat.loc.wrap(name), val_info)?;
        }
      }
      val_env.into()
    }
    Dec::Fun(ty_vars, fval_binds) => {
      if let Some(tv) = ty_vars.first() {
        return Err(tv.loc.wrap(Error::Todo));
      }
      let mut fun_infos = HashMap::with_capacity(fval_binds.len());
      for fval_bind in fval_binds {
        let first = fval_bind.cases.first().unwrap();
        let info = FunInfo {
          args: first.pats.iter().map(|_| st.new_ty_var(false)).collect(),
          ret: st.new_ty_var(false),
        };
        env_ins(&mut fun_infos, first.vid, info)?;
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
            return Err(case.vid.loc.wrap(err));
          }
          let mut pats_val_env = ValEnv::new();
          let mut arg_pat = Vec::with_capacity(info.args.len());
          for (idx, (pat, &tv)) in case.pats.iter().zip(info.args.iter()).enumerate() {
            let (ve, pat_ty, new_pat) = pat::ck(cx, st, pat)?;
            st.subst.unify(pat.loc, Ty::Var(tv), pat_ty)?;
            env_merge(&mut pats_val_env, ve, pat.loc)?;
            tuple_lab(idx);
            arg_pat.push(new_pat);
          }
          let begin = case.pats.first().unwrap().loc;
          let end = case.pats.last().unwrap().loc;
          arg_pats.push(begin.span(end).wrap(Pat::record(arg_pat)));
          if let Some(ty) = &case.ret_ty {
            let new_ty = ty::ck(cx, st, ty)?;
            st.subst.unify(ty.loc, Ty::Var(info.ret), new_ty)?;
          }
          let mut cx = cx.clone();
          // no dupe checking here - intentionally shadow.
          cx.env.val_env.extend(fun_infos_to_ve(&fun_infos));
          cx.env.val_env.extend(pats_val_env);
          let body_ty = ck_exp(&cx, st, &case.body)?;
          st.subst.unify(case.body.loc, Ty::Var(info.ret), body_ty)?;
        }
        let begin = fval_bind.cases.first().unwrap().vid.loc;
        let end = fval_bind.cases.last().unwrap().body.loc;
        exhaustive::ck_match(arg_pats, begin.span(end))?;
      }
      let mut val_env = fun_infos_to_ve(&fun_infos);
      for (_, val_info) in val_env.iter_mut() {
        val_info.ty_scheme.ty.apply(&st.subst);
        generalize(&cx.env.ty_env, &st.sym_tys, &mut val_info.ty_scheme);
      }
      val_env.into()
    }
    Dec::Type(ty_binds) => {
      let mut ty_env = TyEnv::default();
      for ty_bind in ty_binds {
        if let Some(tv) = ty_bind.ty_vars.first() {
          return Err(tv.loc.wrap(Error::Todo));
        }
        let ty = ty::ck(cx, st, &ty_bind.ty)?;
        let info = TyInfo::Alias(TyScheme::mono(ty));
        if ty_env.inner.insert(ty_bind.ty_con.val, info).is_some() {
          return Err(
            ty_bind
              .ty_con
              .loc
              .wrap(Error::Redefined(ty_bind.ty_con.val)),
          );
        }
      }
      ty_env.into()
    }
    Dec::Datatype(dat_binds, ty_binds) => {
      if let Some(tb) = ty_binds.first() {
        return Err(tb.ty_con.loc.wrap(Error::Todo));
      }
      ck_dat_binds(cx.clone(), st, dat_binds)?
    }
    Dec::DatatypeCopy(ty_con, long) => ck_dat_copy(cx, st, *ty_con, long)?,
    Dec::Abstype(..) => return Err(dec.loc.wrap(Error::Todo)),
    Dec::Exception(ex_binds) => {
      let mut val_env = ValEnv::new();
      for ex_bind in ex_binds {
        let val_info = match &ex_bind.inner {
          ExBindInner::Ty(ty) => match ty {
            None => ValInfo::exn(),
            Some(ty) => ValInfo::exn_fn(ty::ck(cx, st, ty)?),
          },
          ExBindInner::Long(vid) => {
            let val_info = get_val_info(get_env(&cx.env, vid)?, vid.last)?;
            if !val_info.id_status.is_exn() {
              let err = Error::ExnWrongIdStatus(val_info.id_status);
              return Err(vid.loc().wrap(err));
            }
            val_info.clone()
          }
        };
        env_ins(&mut val_env, ex_bind.vid, val_info)?;
      }
      val_env.into()
    }
    Dec::Local(fst, snd) => {
      let fst_env = ck(cx, st, fst)?;
      let mut cx = cx.clone();
      cx.o_plus(fst_env);
      ck(&cx, st, snd)?
    }
    Dec::Open(longs) => {
      let mut env = Env::default();
      for long in longs {
        env.extend(get_env(&cx.env, long)?.clone());
      }
      env
    }
    Dec::Seq(decs) => {
      // TODO clone in loop - expensive?
      let mut cx = cx.clone();
      let mut ret = Env::default();
      for dec in decs {
        cx.o_plus(ret.clone());
        ret.extend(ck(&cx, st, dec)?);
      }
      ret
    }
    Dec::Infix(..) | Dec::Infixr(..) | Dec::Nonfix(..) => Env::default(),
  };
  Ok(ret)
}

pub fn ck_dat_binds(mut cx: Cx, st: &mut State, dat_binds: &[DatBind<StrRef>]) -> Result<Env> {
  // these two are across all dat_binds.
  let mut ty_env = TyEnv::default();
  let mut val_env = ValEnv::new();
  for dat_bind in dat_binds {
    if let Some(tv) = dat_bind.ty_vars.first() {
      return Err(tv.loc.wrap(Error::Todo));
    }
    // create a new symbol for the type being generated with this DatBind.
    let sym = st.new_sym(dat_bind.ty_con);
    // tell the original context as well as the overall TyEnv that we return that this new
    // datatype does exist, but tell the State that it has just an empty ValEnv. also perform
    // dupe checking on the name of the new type and assert for sanity checking after the dupe
    // check.
    env_ins(&mut cx.env.ty_env.inner, dat_bind.ty_con, TyInfo::Sym(sym))?;
    // no assert is_none since we may be shadowing something from an earlier Dec in this Cx.
    cx.ty_names.insert(dat_bind.ty_con.val);
    assert!(ty_env
      .inner
      .insert(dat_bind.ty_con.val, TyInfo::Sym(sym))
      .is_none());
    assert!(st
      .sym_tys
      .insert(
        sym,
        SymTyInfo {
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
        ty = Ty::Arrow(ty::ck(&cx, st, arg_ty)?.into(), ty.into());
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
    // now the ValEnv is complete, so we may update st.sym_tys with the true definition of
    // this datatype. assert to check that we inserted the fake answer earlier.
    assert!(st
      .sym_tys
      .insert(
        sym,
        SymTyInfo {
          ty_fcn: TyScheme::mono(Ty::Ctor(Vec::new(), sym)),
          val_env: bind_val_env,
        },
      )
      .is_some());
  }
  Ok(Env {
    ty_env,
    val_env,
    str_env: StrEnv::new(),
  })
}

pub fn ck_dat_copy(
  cx: &Cx,
  st: &mut State,
  ty_con: Located<StrRef>,
  long: &Long<StrRef>,
) -> Result<Env> {
  let sym = match get_ty_info(get_env(&cx.env, long)?, long.last)? {
    TyInfo::Alias(_) => return Err(long.loc().wrap(Error::DatatypeCopyNotDatatype)),
    TyInfo::Sym(sym) => *sym,
  };
  let dt_info = st.sym_tys.get(&sym).unwrap();
  Ok(Env {
    str_env: StrEnv::new(),
    ty_env: TyEnv {
      inner: hashmap![ty_con.val => TyInfo::Sym(sym)],
    },
    val_env: dt_info.val_env.clone(),
  })
}
