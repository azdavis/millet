//! The static standard library.

use crate::intern::StrRef;
use crate::statics::types::{
  Basis, Env, FunEnv, SigEnv, State, StrEnv, Sym, Ty, TyEnv, TyInfo, TyScheme, ValEnv, ValInfo,
};
use std::collections::BTreeMap;

/// Given `t`, returns `t ref`.
fn ref_ty(t: Ty) -> Ty {
  Ty::Ctor(vec![t], Sym::REF)
}

fn bool_val_env() -> ValEnv {
  ValEnv::from([
    (StrRef::TRUE, ValInfo::ctor(TyScheme::mono(Ty::BOOL))),
    (StrRef::FALSE, ValInfo::ctor(TyScheme::mono(Ty::BOOL))),
  ])
}

fn list_val_env(st: &mut State) -> ValEnv {
  let a = st.new_ty_var(false);
  let nil = ValInfo::ctor(TyScheme {
    ty_vars: vec![a],
    ty: Ty::list(Ty::Var(a)),
    overload: None,
  });
  let a = st.new_ty_var(false);
  let cons = ValInfo::ctor(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(
      Ty::pair(Ty::Var(a), Ty::list(Ty::Var(a))).into(),
      Ty::list(Ty::Var(a)).into(),
    ),
    overload: None,
  });
  ValEnv::from([(StrRef::NIL, nil), (StrRef::CONS, cons)])
}

fn ref_val_env(st: &mut State) -> ValEnv {
  let a = st.new_ty_var(false);
  let ref_ = ValInfo::ctor(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::Var(a).into(), ref_ty(Ty::Var(a)).into()),
    overload: None,
  });
  ValEnv::from([(StrRef::REF, ref_)])
}

fn order_val_env() -> ValEnv {
  ValEnv::from([
    (StrRef::LESS, ValInfo::ctor(TyScheme::mono(Ty::ORDER))),
    (StrRef::EQUAL, ValInfo::ctor(TyScheme::mono(Ty::ORDER))),
    (StrRef::GREATER, ValInfo::ctor(TyScheme::mono(Ty::ORDER))),
  ])
}

fn overloaded(st: &mut State, overloads: Vec<Sym>) -> ValInfo {
  let a = st.new_ty_var(false);
  ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::pair(Ty::Var(a), Ty::Var(a)).into(), Ty::Var(a).into()),
    overload: Some(overloads),
  })
}

fn overloaded_one(st: &mut State, overloads: Vec<Sym>) -> ValInfo {
  let a = st.new_ty_var(false);
  ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::Var(a).into(), Ty::Var(a).into()),
    overload: Some(overloads),
  })
}

fn overloaded_cmp(st: &mut State) -> ValInfo {
  let a = st.new_ty_var(false);
  ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::pair(Ty::Var(a), Ty::Var(a)).into(), Ty::BOOL.into()),
    overload: Some(vec![Sym::INT, Sym::WORD, Sym::REAL, Sym::STRING, Sym::CHAR]),
  })
}

fn base_ty(ty: Ty, equality: bool) -> TyInfo {
  TyInfo {
    ty_fcn: TyScheme::mono(ty),
    val_env: ValEnv::new(),
    equality,
  }
}

pub fn get() -> (Basis, State) {
  let real_int = || vec![Sym::INT, Sym::REAL];
  let word_int = || vec![Sym::INT, Sym::WORD];
  let num = || vec![Sym::INT, Sym::WORD, Sym::REAL];
  let real = || vec![Sym::REAL];
  let mut st = State::default();
  st.tys.insert(
    Sym::BOOL,
    TyInfo {
      ty_fcn: TyScheme::mono(Ty::BOOL),
      val_env: bool_val_env(),
      equality: true,
    },
  );
  let a = st.new_ty_var(false);
  let val_env = list_val_env(&mut st);
  st.tys.insert(
    Sym::LIST,
    TyInfo {
      ty_fcn: TyScheme {
        ty_vars: vec![a],
        ty: Ty::list(Ty::Var(a)),
        overload: None,
      },
      val_env,
      equality: true,
    },
  );
  let a = st.new_ty_var(false);
  let val_env = ref_val_env(&mut st);
  st.tys.insert(
    Sym::REF,
    TyInfo {
      ty_fcn: TyScheme {
        ty_vars: vec![a],
        ty: ref_ty(Ty::Var(a)),
        overload: None,
      },
      val_env,
      equality: true,
    },
  );
  st.tys.insert(
    Sym::ORDER,
    TyInfo {
      ty_fcn: TyScheme::mono(Ty::ORDER),
      val_env: order_val_env(),
      equality: true,
    },
  );
  let a = st.new_ty_var(false);
  let assign = ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(
      Ty::pair(ref_ty(Ty::Var(a)), Ty::Var(a)).into(),
      Ty::Record(BTreeMap::new()).into(),
    ),
    overload: None,
  });
  let a = st.new_ty_var(true);
  let eq = ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::pair(Ty::Var(a), Ty::Var(a)).into(), Ty::BOOL.into()),
    overload: None,
  });
  st.tys.insert(Sym::INT, base_ty(Ty::INT, true));
  st.tys.insert(Sym::REAL, base_ty(Ty::REAL, false));
  st.tys.insert(Sym::STRING, base_ty(Ty::STRING, true));
  st.tys.insert(Sym::CHAR, base_ty(Ty::CHAR, true));
  st.tys.insert(Sym::WORD, base_ty(Ty::WORD, true));
  st.tys.insert(Sym::EXN, base_ty(Ty::EXN, false));
  let unit = Ty::Record(BTreeMap::new());
  st.tys.insert(Sym::UNIT, base_ty(unit, false));
  let bs = Basis {
    fun_env: FunEnv::new(),
    sig_env: SigEnv::new(),
    env: Env {
      str_env: StrEnv::new(),
      ty_env: TyEnv {
        inner: BTreeMap::from([
          (StrRef::UNIT, Sym::UNIT),
          (StrRef::BOOL, Sym::BOOL),
          (StrRef::INT, Sym::INT),
          (StrRef::REAL, Sym::REAL),
          (StrRef::STRING, Sym::STRING),
          (StrRef::CHAR, Sym::CHAR),
          (StrRef::WORD, Sym::WORD),
          (StrRef::LIST, Sym::LIST),
          (StrRef::REF, Sym::REF),
          (StrRef::EXN, Sym::EXN),
          (StrRef::ORDER, Sym::ORDER),
        ]),
      },
      val_env: bool_val_env()
        .into_iter()
        .chain(list_val_env(&mut st))
        .chain(ref_val_env(&mut st))
        .chain(order_val_env())
        .chain(ValEnv::from([
          (StrRef::EQ, eq),
          (StrRef::ASSIGN, assign),
          (StrRef::MATCH, ValInfo::exn()),
          (StrRef::BIND, ValInfo::exn()),
          (StrRef::ABS, overloaded_one(&mut st, real_int())),
          (StrRef::TILDE, overloaded_one(&mut st, real_int())),
          (StrRef::DIV, overloaded(&mut st, word_int())),
          (StrRef::MOD, overloaded(&mut st, word_int())),
          (StrRef::STAR, overloaded(&mut st, num())),
          (StrRef::SLASH, overloaded(&mut st, real())),
          (StrRef::PLUS, overloaded(&mut st, num())),
          (StrRef::MINUS, overloaded(&mut st, num())),
          // the Definition states these have type numtxt * numtxt -> numtxt but they really should
          // be numtxt * numtxt -> bool.
          (StrRef::LT, overloaded_cmp(&mut st)),
          (StrRef::GT, overloaded_cmp(&mut st)),
          (StrRef::LT_EQ, overloaded_cmp(&mut st)),
          (StrRef::GT_EQ, overloaded_cmp(&mut st)),
        ]))
        .collect(),
    },
  };
  // sanity check
  for sym in bs.env.ty_env.inner.values() {
    assert!(st.tys.contains_key(sym));
  }
  (bs, st)
}
