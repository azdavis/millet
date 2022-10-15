//! Bases.

use crate::types::{
  BasicOverload, Bs, CompositeOverload, Env, EnvLike as _, EnvStack, FunEnv, IdStatus, Overload,
  RecordTy, SigEnv, StrEnv, Sym, Syms, Ty, TyEnv, TyInfo, TyScheme, TyVarKind, ValEnv, ValInfo,
};
use fast_hash::map;

/// A basis.
#[derive(Debug, Default, Clone)]
pub struct Basis {
  pub(crate) inner: Bs,
}

impl Basis {
  /// Append other onto self.
  pub fn append(&mut self, other: Self) {
    self.inner.append(other.inner);
  }

  /// Adds the structure named `other_name` from `other` into `self` with the name `name`, or
  /// returns `false` if this was not possible.
  pub fn add_str(
    &mut self,
    name: str_util::Name,
    other: &Self,
    other_name: &str_util::Name,
  ) -> bool {
    let env = match other.inner.env.get_str(other_name) {
      Some(x) => x.clone(),
      None => return false,
    };
    self.inner.env.push(Env { str_env: map([(name, env)]), ..Default::default() });
    true
  }

  /// Adds the signature named `other_name` from `other` into `self` with the name `name`, or
  /// returns `false` if this was not possible.
  pub fn add_sig(
    &mut self,
    name: str_util::Name,
    other: &Self,
    other_name: &str_util::Name,
  ) -> bool {
    let env = match other.inner.sig_env.get(other_name) {
      Some(x) => x.clone(),
      None => return false,
    };
    self.inner.as_mut_sig_env().insert(name, env);
    true
  }

  /// Adds the functor named `other_name` from `other` into `self` with the name `name`, or
  /// returns `false` if this was not possible.
  pub fn add_fun(
    &mut self,
    name: str_util::Name,
    other: &Self,
    other_name: &str_util::Name,
  ) -> bool {
    let env = match other.inner.fun_env.get(other_name) {
      Some(x) => x.clone(),
      None => return false,
    };
    self.inner.as_mut_fun_env().insert(name, env);
    true
  }
}

/// Returns the minimal basis and symbols.
///
/// This is distinct from `std_basis` in analysis. This (mostly) just has the definitions that can't
/// be expressed with regular SML files, like `int` and `real` and `string`. Also `bool` and `list`
/// because rebinding their constructor names is forbidden.
#[must_use]
pub fn minimal() -> (Syms, Basis) {
  let mut syms = Syms::default();
  for sym in [Sym::INT, Sym::WORD, Sym::REAL, Sym::CHAR, Sym::STRING] {
    insert_special(&mut syms, sym, basic_datatype(Ty::zero(sym), &[]));
  }
  syms.overloads().int.push(Sym::INT);
  syms.overloads().word.push(Sym::WORD);
  syms.overloads().real.push(Sym::REAL);
  syms.overloads().char.push(Sym::CHAR);
  syms.overloads().string.push(Sym::STRING);
  insert_special(&mut syms, Sym::BOOL, basic_datatype(Ty::BOOL, &["true", "false"]));
  let list_info = {
    let list = |a: Ty| Ty::Con(vec![a], Sym::LIST);
    let alpha_list = TyScheme::one(|a| (list(a), None));
    let cons = TyScheme::one(|a| (Ty::fun(pair(a.clone(), list(a.clone())), list(a)), None));
    TyInfo {
      ty_scheme: alpha_list.clone(),
      val_env: datatype_ve([("nil", alpha_list), ("::", cons)]),
      def: None,
    }
  };
  insert_special(&mut syms, Sym::LIST, list_info);
  let ref_info = {
    let ref_ = |a: Ty| Ty::Con(vec![a], Sym::REF);
    let con = TyScheme::one(|a| (Ty::fun(a.clone(), ref_(a)), None));
    TyInfo {
      ty_scheme: TyScheme::one(|a| (ref_(a), None)),
      val_env: datatype_ve([("ref", con)]),
      def: None,
    }
  };
  insert_special(&mut syms, Sym::REF, ref_info);
  let aliases = [("unit", Ty::Record(RecordTy::new())), ("exn", Ty::EXN)];
  let ty_env: TyEnv = syms
    .iter()
    .map(|(a, b)| (a.clone(), b.clone()))
    .chain(aliases.into_iter().map(|(name, ty)| {
      let ti = TyInfo { ty_scheme: TyScheme::zero(ty), val_env: ValEnv::default(), def: None };
      (str_util::Name::new(name), ti)
    }))
    .collect();
  let fns = {
    let num_pair_to_num =
      overloaded(Overload::Composite(CompositeOverload::Num), |a| Ty::fun(dup(a.clone()), a));
    let real_pair_to_real =
      overloaded(Overload::Basic(BasicOverload::Real), |a| Ty::fun(dup(a.clone()), a));
    let numtxt_pair_to_bool =
      overloaded(Overload::Composite(CompositeOverload::NumTxt), |a| Ty::fun(dup(a), Ty::BOOL));
    let realint_to_realint =
      overloaded(Overload::Composite(CompositeOverload::RealInt), |a| Ty::fun(a.clone(), a));
    let wordint_pair_to_wordint =
      overloaded(Overload::Composite(CompositeOverload::WordInt), |a| Ty::fun(dup(a.clone()), a));
    let equality_pair_to_bool = TyScheme::one(|a| {
      let t = Ty::fun(dup(a), Ty::BOOL);
      (t, Some(TyVarKind::Equality))
    });
    [
      ("*", num_pair_to_num.clone()),
      ("+", num_pair_to_num.clone()),
      ("-", num_pair_to_num),
      ("/", real_pair_to_real),
      ("<", numtxt_pair_to_bool.clone()),
      ("<=", numtxt_pair_to_bool.clone()),
      (">", numtxt_pair_to_bool.clone()),
      (">=", numtxt_pair_to_bool),
      ("~", realint_to_realint.clone()),
      ("abs", realint_to_realint),
      ("div", wordint_pair_to_wordint.clone()),
      ("mod", wordint_pair_to_wordint),
      ("=", equality_pair_to_bool.clone()),
      ("<>", equality_pair_to_bool),
    ]
  };
  let val_env: ValEnv = ty_env
    .values()
    .flat_map(|ti| ti.val_env.iter().map(|(a, b)| (a.clone(), b.clone())))
    .chain(fns.into_iter().map(|(name, ty_scheme)| {
      let vi = ValInfo { ty_scheme, id_status: IdStatus::Val, def: None };
      (str_util::Name::new(name), vi)
    }))
    .collect();
  let basis = Basis {
    inner: Bs {
      fun_env: FunEnv::default().into(),
      sig_env: SigEnv::default().into(),
      env: EnvStack::one(Env { str_env: StrEnv::default(), ty_env, val_env, def: None }),
    },
  };
  (syms, basis)
}

fn insert_special(syms: &mut Syms, sym: Sym, ty_info: TyInfo) {
  let started = syms.start(str_util::Name::new(sym.special().unwrap()));
  assert_eq!(sym, started.sym());
  syms.finish(started, ty_info);
}

fn basic_datatype(ty: Ty, ctors: &[&str]) -> TyInfo {
  let ty_scheme = TyScheme::zero(ty);
  let val_env = datatype_ve(ctors.iter().map(|&x| (x, ty_scheme.clone())));
  TyInfo { ty_scheme, val_env, def: None }
}

fn datatype_ve<'a, I>(xs: I) -> ValEnv
where
  I: IntoIterator<Item = (&'a str, TyScheme)>,
{
  xs.into_iter()
    .map(|(name, ty_scheme)| {
      (str_util::Name::new(name), ValInfo { ty_scheme, id_status: IdStatus::Con, def: None })
    })
    .collect()
}

fn overloaded<F>(ov: Overload, f: F) -> TyScheme
where
  F: FnOnce(Ty) -> Ty,
{
  TyScheme::one(|a| (f(a), Some(TyVarKind::Overloaded(ov))))
}

fn dup(ty: Ty) -> Ty {
  pair(ty.clone(), ty)
}

fn pair(t1: Ty, t2: Ty) -> Ty {
  Ty::Record(RecordTy::from([(sml_hir::Lab::Num(1), t1), (sml_hir::Lab::Num(2), t2)]))
}
