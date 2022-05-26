use crate::types::{Cx, Env, StrEnv, Sym, Syms, Ty, TyInfo, TyScheme, ValEnv};
use fast_hash::FxHashMap;
use std::collections::BTreeMap;

const BUILTIN: [Sym; 10] = [
  Sym::BOOL,
  Sym::CHAR,
  Sym::INT,
  Sym::REAL,
  Sym::STRING,
  Sym::WORD,
  Sym::EXN,
  Sym::REF,
  Sym::LIST,
  Sym::ORDER,
];

pub(crate) fn get() -> (Syms, Cx) {
  let syms = Syms::standard_basis();
  let ty_env: FxHashMap<_, _> = BUILTIN
    .iter()
    .map(|s| (syms.get_name(s).clone(), syms.get(s).clone()))
    .chain(std::iter::once((
      hir::Name::new("unit"),
      TyInfo {
        ty_scheme: TyScheme::mono(Ty::Record(BTreeMap::new())),
        val_env: ValEnv::default(),
      },
    )))
    .collect();
  let val_env: FxHashMap<_, _> = ty_env
    .values()
    .flat_map(|ti| ti.val_env.iter().map(|(a, b)| (a.clone(), b.clone())))
    .chain([])
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
