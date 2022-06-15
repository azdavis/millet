//! This is distinct from std_basis in analysis. This (mostly) just has the definitions that can't
//! be expressed with regular SML files, like `int` and `real` and `string`. Also `bool` and `list`
//! because rebinding their constructor names is forbidden.

use crate::types::{
  Bs, Env, FunEnv, IdStatus, Overload, SigEnv, StrEnv, Sym, Syms, Ty, TyEnv, TyInfo, TyScheme,
  TyVarKind, ValEnv, ValInfo,
};
use std::collections::BTreeMap;
use std::sync::Arc;

pub(crate) fn get() -> (Syms, Bs) {
  let mut syms = Syms::default();
  for sym in [Sym::INT, Sym::WORD, Sym::REAL, Sym::CHAR, Sym::STRING] {
    insert_special(&mut syms, sym, basic_datatype(Ty::zero(sym), &[]));
  }
  insert_special(
    &mut syms,
    Sym::BOOL,
    basic_datatype(Ty::BOOL, &["true", "false"]),
  );
  let list_info = {
    let list = |a: Ty| Ty::Con(vec![a], Sym::LIST);
    let alpha_list = TyScheme::one(|a| (list(a), None));
    let cons = TyScheme::one(|a| (Ty::fun(pair(a.clone(), list(a.clone())), list(a)), None));
    TyInfo {
      ty_scheme: alpha_list.clone(),
      val_env: datatype_ve([("nil", alpha_list), ("::", cons)]),
    }
  };
  insert_special(&mut syms, Sym::LIST, list_info);
  let ref_info = {
    let ref_ = |a: Ty| Ty::Con(vec![a], Sym::REF);
    let con = TyScheme::one(|a| (Ty::fun(a.clone(), ref_(a)), None));
    TyInfo {
      ty_scheme: TyScheme::one(|a| (ref_(a), None)),
      val_env: datatype_ve([("ref", con)]),
    }
  };
  insert_special(&mut syms, Sym::REF, ref_info);
  let aliases = [("unit", unit()), ("exn", Ty::EXN)];
  let ty_env: TyEnv = syms
    .iter()
    .map(|(a, b)| (a.clone(), b.clone()))
    .chain(aliases.into_iter().map(|(name, ty)| {
      let ti = TyInfo {
        ty_scheme: TyScheme::zero(ty),
        val_env: ValEnv::default(),
      };
      (hir::Name::new(name), ti)
    }))
    .collect();
  let fns = {
    let realint_to_realint = overloaded(Overload::RealInt, |a| Ty::fun(a.clone(), a));
    let wordint_pair_to_wordint = overloaded(Overload::WordInt, |a| Ty::fun(dup(a.clone()), a));
    let num_pair_to_num = overloaded(Overload::Num, |a| Ty::fun(dup(a.clone()), a));
    let numtxt_pair_to_bool = overloaded(Overload::NumTxt, |a| Ty::fun(dup(a), Ty::BOOL));
    let real_pair_to_real = TyScheme::zero(Ty::fun(dup(Ty::REAL), Ty::REAL));
    let assign = TyScheme::one(|a| {
      let t = Ty::fun(pair(Ty::Con(vec![a.clone()], Sym::REF), a), unit());
      (t, None)
    });
    let eq = TyScheme::one(|a| {
      let t = Ty::fun(dup(a), Ty::BOOL);
      (t, Some(TyVarKind::Equality))
    });
    [
      ("abs", realint_to_realint.clone()),
      ("~", realint_to_realint),
      ("div", wordint_pair_to_wordint.clone()),
      ("mod", wordint_pair_to_wordint),
      ("*", num_pair_to_num.clone()),
      ("/", real_pair_to_real),
      ("+", num_pair_to_num.clone()),
      ("-", num_pair_to_num),
      ("<", numtxt_pair_to_bool.clone()),
      (">", numtxt_pair_to_bool.clone()),
      ("<=", numtxt_pair_to_bool.clone()),
      (">=", numtxt_pair_to_bool),
      (":=", assign),
      ("=", eq),
    ]
  };
  let val_env: ValEnv = ty_env
    .values()
    .flat_map(|ti| ti.val_env.iter().map(|(a, b)| (a.clone(), b.clone())))
    .chain(fns.into_iter().map(|(name, ty_scheme)| {
      let vi = ValInfo {
        ty_scheme,
        id_status: IdStatus::Val,
      };
      (hir::Name::new(name), vi)
    }))
    .collect();
  let bs = Bs {
    fun_env: FunEnv::default(),
    sig_env: SigEnv::default(),
    env: Arc::new(Env {
      str_env: StrEnv::default(),
      ty_env,
      val_env,
    }),
  };
  (syms, bs)
}

fn insert_special(syms: &mut Syms, sym: Sym, ty_info: TyInfo) {
  let started = syms.start(hir::Name::new(sym.special().unwrap()));
  assert_eq!(sym, started.sym());
  syms.finish(started, ty_info);
}

fn basic_datatype(ty: Ty, ctors: &[&str]) -> TyInfo {
  let ty_scheme = TyScheme::zero(ty);
  let val_env = datatype_ve(ctors.iter().map(|&x| (x, ty_scheme.clone())));
  TyInfo { ty_scheme, val_env }
}

fn datatype_ve<'a, I>(xs: I) -> ValEnv
where
  I: IntoIterator<Item = (&'a str, TyScheme)>,
{
  xs.into_iter()
    .map(|(name, ty_scheme)| {
      (
        hir::Name::new(name),
        ValInfo {
          ty_scheme,
          id_status: IdStatus::Con,
        },
      )
    })
    .collect()
}

fn overloaded<F>(x: Overload, f: F) -> TyScheme
where
  F: FnOnce(Ty) -> Ty,
{
  TyScheme::one(|a| (f(a), Some(TyVarKind::Overloaded(x))))
}

fn dup(ty: Ty) -> Ty {
  pair(ty.clone(), ty)
}

fn pair(t1: Ty, t2: Ty) -> Ty {
  Ty::Record(BTreeMap::from([
    (hir::Lab::Num(1), t1),
    (hir::Lab::Num(2), t2),
  ]))
}

fn unit() -> Ty {
  Ty::Record(BTreeMap::new())
}
