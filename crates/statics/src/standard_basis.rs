use crate::types::{
  Cx, Env, IdStatus, Overload, StrEnv, Sym, Syms, Ty, TyInfo, TyScheme, TyVarKind, ValEnv, ValInfo,
};
use fast_hash::FxHashMap;
use std::collections::BTreeMap;

pub(crate) fn get() -> (Syms, Cx) {
  let mut syms = Syms::standard_basis();
  let builtin = [
    Sym::BOOL,
    Sym::CHAR,
    Sym::INT,
    Sym::REAL,
    Sym::STRING,
    Sym::WORD,
    Sym::REF,
    Sym::LIST,
    Sym::ORDER,
  ];
  let aliases = [
    ("unit", Ty::Record(BTreeMap::new())),
    ("exn", Ty::zero(Sym::EXN)),
  ];
  let ty_env: FxHashMap<_, _> = builtin
    .iter()
    .map(|s| {
      let (name, info) = syms.get(s).unwrap();
      (name.clone(), info.clone())
    })
    .chain(aliases.into_iter().map(|(name, ty)| {
      let vi = TyInfo {
        ty_scheme: TyScheme::zero(ty),
        val_env: ValEnv::default(),
      };
      (hir::Name::new(name), vi)
    }))
    .collect();
  let fns = {
    let realint_to_realint = TyScheme::one(|a| (Ty::fun(a.clone(), a), ov(Overload::RealInt)));
    let wordint_pair_to_wordint =
      TyScheme::one(|a| (Ty::fun(dup(a.clone()), a), ov(Overload::WordInt)));
    let num_pair_to_num = TyScheme::one(|a| (Ty::fun(dup(a.clone()), a), ov(Overload::Num)));
    let numtxt_pair_to_bool =
      TyScheme::one(|a| (Ty::fun(dup(a), Ty::zero(Sym::BOOL)), ov(Overload::NumTxt)));
    let real_pair_to_real = TyScheme::zero(Ty::fun(dup(Ty::zero(Sym::REAL)), Ty::zero(Sym::REAL)));
    let assign = TyScheme::one(|a| {
      (
        Ty::fun(
          pair(Ty::Con(vec![a.clone()], Sym::REF), a),
          Ty::Record(BTreeMap::new()),
        ),
        None,
      )
    });
    let eq = TyScheme::one(|a| {
      (
        Ty::fun(dup(a), Ty::zero(Sym::BOOL)),
        Some(TyVarKind::Equality),
      )
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
  let exns = [
    ("Match", None),
    ("Bind", None),
    // not actually part of the standard basis according to the Definition
    ("Fail", Some(Ty::zero(Sym::STRING))),
  ];
  let val_env: FxHashMap<_, _> = ty_env
    .values()
    .flat_map(|ti| ti.val_env.iter().map(|(a, b)| (a.clone(), b.clone())))
    .chain(fns.into_iter().map(|(name, ty_scheme)| {
      let vi = ValInfo {
        ty_scheme,
        id_status: IdStatus::Val,
      };
      (hir::Name::new(name), vi)
    }))
    .chain(exns.into_iter().map(|(name, param)| {
      let mut ty = Ty::zero(Sym::EXN);
      if let Some(ref param) = param {
        ty = Ty::fun(param.clone(), ty);
      }
      let name = hir::Name::new(name);
      let exn = syms.insert_exn(name.clone(), param);
      let vi = ValInfo {
        ty_scheme: TyScheme::zero(ty),
        id_status: IdStatus::Exn(exn),
      };
      (name, vi)
    }))
    .collect();
  let cx = Cx {
    env: Env {
      str_env: StrEnv::default(),
      ty_env,
      val_env,
    },
    ty_vars: FxHashMap::default(),
  };
  (syms, cx)
}

fn ov(x: Overload) -> Option<TyVarKind> {
  Some(TyVarKind::Overloaded(x))
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
