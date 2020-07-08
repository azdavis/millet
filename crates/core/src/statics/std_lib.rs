//! The static standard library.

use crate::ast::Label;
use crate::intern::StrRef;
use crate::statics::types::{
  Basis, Env, FunEnv, SigEnv, State, StrEnv, Sym, SymTyInfo, Ty, TyEnv, TyInfo, TyScheme, ValEnv,
  ValInfo,
};
use maplit::{hashmap, hashset};

fn bool_val_env() -> ValEnv {
  hashmap![
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
      Ty::Record(vec![
        (Label::Num(1), Ty::Var(a)),
        (Label::Num(2), Ty::list(Ty::Var(a))),
      ])
      .into(),
      Ty::list(Ty::Var(a)).into(),
    ),
    overload: None,
  });
  hashmap![StrRef::NIL => nil, StrRef::CONS => cons]
}

fn ref_val_env(st: &mut State) -> ValEnv {
  let a = st.new_ty_var(false);
  let ref_ = ValInfo::ctor(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(Ty::Var(a).into(), Ty::ref_(Ty::Var(a)).into()),
    overload: None,
  });
  hashmap![StrRef::REF => ref_]
}

fn order_val_env() -> ValEnv {
  hashmap![
    StrRef::LESS => ValInfo::ctor(TyScheme::mono(Ty::ORDER)),
    StrRef::EQUAL => ValInfo::ctor(TyScheme::mono(Ty::ORDER)),
    StrRef::GREATER => ValInfo::ctor(TyScheme::mono(Ty::ORDER)),
  ]
}

fn overloaded(st: &mut State, overloads: Vec<StrRef>) -> ValInfo {
  let a = st.new_ty_var(false);
  ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(
      Ty::Record(vec![
        (Label::Num(1), Ty::Var(a)),
        (Label::Num(2), Ty::Var(a)),
      ])
      .into(),
      Ty::Var(a).into(),
    ),
    overload: Some(overloads),
  })
}

fn overloaded_cmp(st: &mut State) -> ValInfo {
  let a = st.new_ty_var(false);
  ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(
      Ty::Record(vec![
        (Label::Num(1), Ty::Var(a)),
        (Label::Num(2), Ty::Var(a)),
      ])
      .into(),
      Ty::BOOL.into(),
    ),
    overload: Some(vec![
      StrRef::INT,
      StrRef::WORD,
      StrRef::REAL,
      StrRef::STRING,
      StrRef::CHAR,
    ]),
  })
}

pub fn get() -> (Basis, State) {
  let real_int = || vec![StrRef::INT, StrRef::REAL];
  let word_int = || vec![StrRef::INT, StrRef::WORD];
  let num = || vec![StrRef::INT, StrRef::WORD, StrRef::REAL];
  let mut st = State::default();
  st.sym_tys.insert(
    Sym::base(StrRef::BOOL),
    SymTyInfo {
      ty_fcn: TyScheme::mono(Ty::BOOL),
      val_env: bool_val_env(),
    },
  );
  let a = st.new_ty_var(false);
  let val_env = list_val_env(&mut st);
  st.sym_tys.insert(
    Sym::base(StrRef::LIST),
    SymTyInfo {
      ty_fcn: TyScheme {
        ty_vars: vec![a],
        ty: Ty::list(Ty::Var(a)),
        overload: None,
      },
      val_env,
    },
  );
  let a = st.new_ty_var(false);
  let val_env = ref_val_env(&mut st);
  st.sym_tys.insert(
    Sym::base(StrRef::REF),
    SymTyInfo {
      ty_fcn: TyScheme {
        ty_vars: vec![a],
        ty: Ty::ref_(Ty::Var(a)),
        overload: None,
      },
      val_env,
    },
  );
  st.sym_tys.insert(
    Sym::base(StrRef::ORDER),
    SymTyInfo {
      ty_fcn: TyScheme::mono(Ty::ORDER),
      val_env: order_val_env(),
    },
  );
  let a = st.new_ty_var(false);
  let assign = ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(
      Ty::Record(vec![
        (
          Label::Num(1),
          Ty::Ctor(vec![Ty::Var(a)], Sym::base(StrRef::REF)),
        ),
        (Label::Num(2), Ty::Var(a)),
      ])
      .into(),
      Ty::Record(Vec::new()).into(),
    ),
    overload: None,
  });
  let a = st.new_ty_var(true);
  let eq = ValInfo::val(TyScheme {
    ty_vars: vec![a],
    ty: Ty::Arrow(
      Ty::Record(vec![
        (Label::Num(1), Ty::Var(a)),
        (Label::Num(2), Ty::Var(a)),
      ])
      .into(),
      Ty::BOOL.into(),
    ),
    overload: None,
  });
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
        inner: hashmap![
          StrRef::UNIT => TyInfo::Alias(TyScheme::mono(Ty::Record(Vec::new()))),
          StrRef::BOOL => TyInfo::Sym(Sym::base(StrRef::BOOL)),
          StrRef::INT => TyInfo::Alias(TyScheme::mono(Ty::INT)),
          StrRef::REAL => TyInfo::Alias(TyScheme::mono(Ty::REAL)),
          StrRef::STRING => TyInfo::Alias(TyScheme::mono(Ty::STRING)),
          StrRef::CHAR => TyInfo::Alias(TyScheme::mono(Ty::CHAR)),
          StrRef::WORD => TyInfo::Alias(TyScheme::mono(Ty::WORD)),
          StrRef::LIST => TyInfo::Sym(Sym::base(StrRef::LIST)),
          StrRef::REF => TyInfo::Sym(Sym::base(StrRef::REF)),
          StrRef::EXN => TyInfo::Alias(TyScheme::mono(Ty::EXN)),
          StrRef::ORDER => TyInfo::Sym(Sym::base(StrRef::ORDER)),
        ],
      },
      val_env: bool_val_env()
        .into_iter()
        .chain(list_val_env(&mut st))
        .chain(ref_val_env(&mut st))
        .chain(order_val_env())
        .chain(hashmap![
          StrRef::EQ => eq,
          StrRef::ASSIGN => assign,
          StrRef::MATCH => ValInfo::exn(),
          StrRef::BIND => ValInfo::exn(),
          StrRef::ABS => overloaded(&mut st, real_int()),
          StrRef::TILDE => overloaded(&mut st, real_int()),
          StrRef::DIV => overloaded(&mut st, word_int()),
          StrRef::MOD => overloaded(&mut st, word_int()),
          StrRef::STAR => overloaded(&mut st, word_int()),
          StrRef::SLASH => overloaded(&mut st, vec![StrRef::REAL]),
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
  (bs, st)
}
