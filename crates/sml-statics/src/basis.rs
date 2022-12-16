//! Bases. (The plural of "basis".)

use crate::def::PrimitiveKind;
use crate::env::{Bs, Env, EnvLike as _, EnvStack, FunEnv, SigEnv, StrEnv};
use crate::types::{
  Basic, Composite, Equality, IdStatus, Overload, RecordTy, Sym, Syms, Ty, TyEnv, TyInfo, TyScheme,
  TyVarKind, ValEnv, ValInfo,
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

  /// Adds the item named `other_name` from `other` into `self` with the name `name`, or
  /// returns `false` if this was not possible.
  pub fn add(
    &mut self,
    ns: sml_namespace::Namespace,
    name: str_util::Name,
    other: &Self,
    other_name: &str_util::Name,
  ) -> bool {
    match ns {
      sml_namespace::Namespace::Structure => match other.inner.env.get_str(other_name) {
        Some(env) => {
          let env = Env { str_env: map([(name, env.clone())]), ..Default::default() };
          self.inner.env.push(env);
          true
        }
        None => false,
      },
      sml_namespace::Namespace::Signature => match other.inner.sig_env.get(other_name) {
        Some(env) => {
          let env = env.clone();
          self.inner.as_mut_sig_env().insert(name, env);
          true
        }
        None => false,
      },
      sml_namespace::Namespace::Functor => match other.inner.fun_env.get(other_name) {
        Some(env) => {
          let env = env.clone();
          self.inner.as_mut_fun_env().insert(name, env);
          true
        }
        None => false,
      },
    }
  }
}

/// Returns the minimal basis and symbols.
///
/// This is distinct from `std_basis` in analysis. This (mostly) just has the definitions that can't
/// be expressed with regular SML files, like `int` and `real` and `string`. Also `bool` and `list`
/// because rebinding their constructor names is forbidden.
///
/// # Panics
///
/// Upon internal error.
#[must_use]
pub fn minimal() -> (Syms, Basis) {
  // @sync(special_sym_order)
  let mut syms = Syms::default();
  for sym in [Sym::INT, Sym::WORD, Sym::REAL, Sym::CHAR, Sym::STRING] {
    insert_special(&mut syms, sym, basic_datatype(sym, &[]));
  }
  syms.overloads_mut().int.push(Sym::INT);
  syms.overloads_mut().word.push(Sym::WORD);
  syms.overloads_mut().real.push(Sym::REAL);
  syms.overloads_mut().char.push(Sym::CHAR);
  syms.overloads_mut().string.push(Sym::STRING);
  insert_special(
    &mut syms,
    Sym::BOOL,
    basic_datatype(Sym::BOOL, &[PrimitiveKind::True, PrimitiveKind::False]),
  );
  let list_info = {
    let list = |a: Ty| Ty::Con(vec![a], Sym::LIST);
    let alpha_list = TyScheme::one(|a| (list(a), None));
    let cons = TyScheme::one(|a| (Ty::fun(pair(a.clone(), list(a.clone())), list(a)), None));
    TyInfo {
      ty_scheme: alpha_list.clone(),
      val_env: datatype_ve([(PrimitiveKind::Nil, alpha_list), (PrimitiveKind::Cons, cons)]),
      def: Some(PrimitiveKind::List.into()),
    }
  };
  insert_special(&mut syms, Sym::LIST, list_info);
  let ref_info = {
    let ref_ = |a: Ty| Ty::Con(vec![a], Sym::REF);
    let con = TyScheme::one(|a| (Ty::fun(a.clone(), ref_(a)), None));
    TyInfo {
      ty_scheme: TyScheme::one(|a| (ref_(a), None)),
      val_env: datatype_ve([(PrimitiveKind::RefVal, con)]),
      def: Some(PrimitiveKind::RefTy.into()),
    }
  };
  insert_special(&mut syms, Sym::REF, ref_info);
  let aliases = [(PrimitiveKind::Unit, unit()), (PrimitiveKind::Exn, Ty::EXN)];
  let ty_env: TyEnv = syms
    .iter_syms()
    .map(|sym_info| {
      assert!(sym_info.path.prefix().is_empty(), "only built-in types are currently in this Syms");
      (sym_info.path.last().clone(), sym_info.ty_info.clone())
    })
    .chain(aliases.into_iter().map(|(name, ty)| {
      let ti = TyInfo {
        ty_scheme: TyScheme::zero(ty),
        val_env: ValEnv::default(),
        def: Some(name.into()),
      };
      (str_util::Name::new(name.as_str()), ti)
    }))
    .collect();
  let fns = {
    let num_pair_to_num =
      overloaded(Overload::Composite(Composite::Num), |a| Ty::fun(dup(a.clone()), a));
    let real_pair_to_real =
      overloaded(Overload::Basic(Basic::Real), |a| Ty::fun(dup(a.clone()), a));
    let numtxt_pair_to_bool =
      overloaded(Overload::Composite(Composite::NumTxt), |a| Ty::fun(dup(a), Ty::BOOL));
    let realint_to_realint =
      overloaded(Overload::Composite(Composite::RealInt), |a| Ty::fun(a.clone(), a));
    let wordint_pair_to_wordint =
      overloaded(Overload::Composite(Composite::WordInt), |a| Ty::fun(dup(a.clone()), a));
    let equality_pair_to_bool = TyScheme::one(|a| {
      let t = Ty::fun(dup(a), Ty::BOOL);
      (t, Some(TyVarKind::Equality))
    });
    [
      (PrimitiveKind::Mul, num_pair_to_num.clone()),
      (PrimitiveKind::Add, num_pair_to_num.clone()),
      (PrimitiveKind::Sub, num_pair_to_num),
      (PrimitiveKind::RealDiv, real_pair_to_real),
      (PrimitiveKind::Lt, numtxt_pair_to_bool.clone()),
      (PrimitiveKind::LtEq, numtxt_pair_to_bool.clone()),
      (PrimitiveKind::Gt, numtxt_pair_to_bool.clone()),
      (PrimitiveKind::GtEq, numtxt_pair_to_bool),
      (PrimitiveKind::Neg, realint_to_realint.clone()),
      (PrimitiveKind::Abs, realint_to_realint),
      (PrimitiveKind::Div, wordint_pair_to_wordint.clone()),
      (PrimitiveKind::Mod, wordint_pair_to_wordint),
      (PrimitiveKind::Eq, equality_pair_to_bool.clone()),
      (PrimitiveKind::Neq, equality_pair_to_bool),
      (PrimitiveKind::Use, TyScheme::zero(Ty::fun(Ty::STRING, unit()))),
    ]
  };
  let val_env: ValEnv = ty_env
    .values()
    .flat_map(|ti| ti.val_env.iter().map(|(a, b)| (a.clone(), b.clone())))
    .chain(fns.into_iter().map(|(name, ty_scheme)| {
      let vi = ValInfo { ty_scheme, id_status: IdStatus::Val, def: Some(name.into()) };
      (str_util::Name::new(name.as_str()), vi)
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
  assert_ne!(sym, Sym::EXN);
  let equality = if sym == Sym::REF {
    Equality::Always
  } else if sym == Sym::REAL {
    Equality::Never
  } else {
    Equality::Sometimes
  };
  let started =
    syms.start(sml_hir::Path::one(str_util::Name::new(sym.primitive().unwrap().as_str())));
  assert_eq!(sym, started.sym());
  syms.finish(started, ty_info, equality);
}

fn basic_datatype(sym: Sym, ctors: &'static [PrimitiveKind]) -> TyInfo {
  let ty_scheme = TyScheme::zero(Ty::zero(sym));
  let val_env = datatype_ve(ctors.iter().map(|&x| (x, ty_scheme.clone())));
  TyInfo { ty_scheme, val_env, def: Some(sym.primitive().unwrap().into()) }
}

fn datatype_ve<I>(xs: I) -> ValEnv
where
  I: IntoIterator<Item = (PrimitiveKind, TyScheme)>,
{
  xs.into_iter()
    .map(|(name, ty_scheme)| {
      let vi = ValInfo { ty_scheme, id_status: IdStatus::Con, def: Some(name.into()) };
      (str_util::Name::new(name.as_str()), vi)
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

fn unit() -> Ty {
  Ty::Record(RecordTy::new())
}
