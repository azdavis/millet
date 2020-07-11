//! The static standard library.

use crate::intern::StrRef;
use crate::statics::types::{
  Basis, Env, FunEnv, SigEnv, State, StrEnv, Sym, SymTyInfo, Ty, TyEnv, TyInfo, TyScheme, ValEnv,
  ValInfo,
};
use maplit::{btreemap, hashmap, hashset};

fn bool_val_env() -> ValEnv {
  btreemap![
    StrRef::TRUE => ValInfo::ctor(TyScheme::mono(Ty::BOOL)),
    StrRef::FALSE => ValInfo::ctor(TyScheme::mono(Ty::BOOL)),
  ]
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
  btreemap![StrRef::NIL => nil, StrRef::CONS => cons]
}

fn ref_val_env(st: &mut State) -> ValEnv {
  let a = st.new_ty_var(false);
  let ref_ = ValInfo::ctor(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::Var(a).into(), Ty::ref_(Ty::Var(a)).into()),
    overload: None,
  });
  btreemap![StrRef::REF => ref_]
}

fn order_val_env() -> ValEnv {
  btreemap![
    StrRef::LESS => ValInfo::ctor(TyScheme::mono(Ty::ORDER)),
    StrRef::EQUAL => ValInfo::ctor(TyScheme::mono(Ty::ORDER)),
    StrRef::GREATER => ValInfo::ctor(TyScheme::mono(Ty::ORDER)),
  ]
}

fn overloaded(st: &mut State, overloads: Vec<Ty>) -> ValInfo {
  let a = st.new_ty_var(false);
  ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::pair(Ty::Var(a), Ty::Var(a)).into(), Ty::Var(a).into()),
    overload: Some(overloads),
  })
}

fn overloaded_cmp(st: &mut State) -> ValInfo {
  let a = st.new_ty_var(false);
  ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::pair(Ty::Var(a), Ty::Var(a)).into(), Ty::BOOL.into()),
    overload: Some(vec![Ty::INT, Ty::WORD, Ty::REAL, Ty::STRING, Ty::CHAR]),
  })
}

fn base_ty(ty: Ty, equality: bool) -> SymTyInfo {
  SymTyInfo {
    ty_fcn: TyScheme::mono(ty),
    val_env: ValEnv::new(),
    equality,
    datatype: false,
  }
}

pub fn get() -> (Basis, State) {
  let real_int = || vec![Ty::INT, Ty::REAL];
  let word_int = || vec![Ty::INT, Ty::WORD];
  let num = || vec![Ty::INT, Ty::WORD, Ty::REAL];
  let mut st = State::default();
  st.sym_tys.insert(
    Sym::BOOL,
    SymTyInfo {
      ty_fcn: TyScheme::mono(Ty::BOOL),
      val_env: bool_val_env(),
      equality: true,
      datatype: true,
    },
  );
  let a = st.new_ty_var(false);
  let val_env = list_val_env(&mut st);
  st.sym_tys.insert(
    Sym::LIST,
    SymTyInfo {
      ty_fcn: TyScheme {
        ty_vars: vec![a],
        ty: Ty::list(Ty::Var(a)),
        overload: None,
      },
      val_env,
      equality: true,
      datatype: true,
    },
  );
  let a = st.new_ty_var(false);
  let val_env = ref_val_env(&mut st);
  st.sym_tys.insert(
    Sym::REF,
    SymTyInfo {
      ty_fcn: TyScheme {
        ty_vars: vec![a],
        ty: Ty::ref_(Ty::Var(a)),
        overload: None,
      },
      val_env,
      equality: true,
      datatype: true,
    },
  );
  st.sym_tys.insert(
    Sym::ORDER,
    SymTyInfo {
      ty_fcn: TyScheme::mono(Ty::ORDER),
      val_env: order_val_env(),
      equality: true,
      datatype: true,
    },
  );
  let a = st.new_ty_var(false);
  let assign = ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(
      Ty::pair(Ty::ref_(Ty::Var(a)), Ty::Var(a)).into(),
      Ty::Record(btreemap![]).into(),
    ),
    overload: None,
  });
  let a = st.new_ty_var(true);
  let eq = ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::pair(Ty::Var(a), Ty::Var(a)).into(), Ty::BOOL.into()),
    overload: None,
  });
  st.sym_tys.extend(hashmap![
    Sym::INT => base_ty(Ty::INT, true),
    Sym::REAL => base_ty(Ty::REAL, false),
    Sym::STRING => base_ty(Ty::STRING, true),
    Sym::CHAR => base_ty(Ty::CHAR, true),
    Sym::WORD => base_ty(Ty::WORD, true),
    Sym::EXN => base_ty(Ty::EXN, false),
  ]);
  let bs = Basis {
    ty_names: hashset![
      StrRef::BOOL,
      StrRef::INT,
      StrRef::REAL,
      StrRef::STRING,
      StrRef::CHAR,
      StrRef::WORD,
      StrRef::LIST,
      StrRef::REF,
      StrRef::EXN,
      StrRef::ORDER,
    ],
    fun_env: FunEnv::new(),
    sig_env: SigEnv::new(),
    env: Env {
      str_env: StrEnv::new(),
      ty_env: TyEnv {
        inner: btreemap![
          StrRef::UNIT => TyInfo::Alias(TyScheme::mono(Ty::Record(btreemap![]))),
          StrRef::BOOL => TyInfo::Sym(Sym::BOOL),
          StrRef::INT => TyInfo::Sym(Sym::INT),
          StrRef::REAL => TyInfo::Sym(Sym::REAL),
          StrRef::STRING => TyInfo::Sym(Sym::STRING),
          StrRef::CHAR => TyInfo::Sym(Sym::CHAR),
          StrRef::WORD => TyInfo::Sym(Sym::WORD),
          StrRef::LIST => TyInfo::Sym(Sym::LIST),
          StrRef::REF => TyInfo::Sym(Sym::REF),
          StrRef::EXN => TyInfo::Sym(Sym::EXN),
          StrRef::ORDER => TyInfo::Sym(Sym::ORDER),
        ],
      },
      val_env: bool_val_env()
        .into_iter()
        .chain(list_val_env(&mut st))
        .chain(ref_val_env(&mut st))
        .chain(order_val_env())
        .chain(btreemap![
          StrRef::EQ => eq,
          StrRef::ASSIGN => assign,
          StrRef::MATCH => ValInfo::exn(),
          StrRef::BIND => ValInfo::exn(),
          StrRef::ABS => overloaded(&mut st, real_int()),
          StrRef::TILDE => overloaded(&mut st, real_int()),
          StrRef::DIV => overloaded(&mut st, word_int()),
          StrRef::MOD => overloaded(&mut st, word_int()),
          StrRef::STAR => overloaded(&mut st, word_int()),
          StrRef::SLASH => overloaded(&mut st, vec![Ty::REAL]),
          StrRef::PLUS => overloaded(&mut st, num()),
          StrRef::MINUS => overloaded(&mut st, num()),
          // the Definition states these have type numtxt * numtxt -> numtxt but they really should
          // be numtxt * numtxt -> bool.
          StrRef::LT => overloaded_cmp(&mut st),
          StrRef::GT => overloaded_cmp(&mut st),
          StrRef::LT_EQ => overloaded_cmp(&mut st),
          StrRef::GT_EQ => overloaded_cmp(&mut st),
        ])
        .collect(),
    },
  };
  // sanity check
  for name in bs.ty_names.iter() {
    assert!(bs.env.ty_env.inner.contains_key(name));
  }
  for ty_info in bs.env.ty_env.inner.values() {
    if let TyInfo::Sym(sym) = ty_info {
      assert!(st.sym_tys.contains_key(sym));
    }
  }
  (bs, st)
}
