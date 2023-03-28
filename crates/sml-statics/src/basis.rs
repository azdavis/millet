//! Bases. (The plural of "basis".)

use crate::env::{Cx, Env, FunEnv, SigEnv, StrEnv};
use crate::types::{
  Equality, IdStatus, RecordTy, Sym, Syms, Ty, TyEnv, TyInfo, TyScheme, TyVarKind, ValEnv, ValInfo,
};
use crate::{def::PrimitiveKind, disallow::Disallow, overload};
use fast_hash::FxHashMap;

/// A basis.
#[derive(Debug, Default, Clone)]
pub struct Bs {
  pub(crate) env: Env,
  pub(crate) sig_env: SigEnv,
  pub(crate) fun_env: FunEnv,
}

impl Bs {
  pub(crate) fn as_cx(&self) -> Cx {
    Cx { env: self.env.clone(), fixed: FxHashMap::default() }
  }

  /// Append other onto self, emptying other.
  pub fn append(&mut self, mut other: Bs) {
    self.env.append(&mut other.env);
    self.sig_env.append(&mut other.sig_env);
    self.fun_env.append(&mut other.fun_env);
  }

  /// Consolidates internal memory for this, so that it will be faster to clone next time.
  pub fn consolidate(&mut self) {
    self.env.consolidate();
    self.sig_env.consolidate();
    self.fun_env.consolidate();
  }

  /// Adds the item named `other_name` from `other` into `self` with the name `name`, or
  /// returns `false` if this was not possible.
  pub fn add(
    &mut self,
    ns: sml_namespace::Module,
    name: str_util::Name,
    other: &Self,
    other_name: &str_util::Name,
  ) -> bool {
    match ns {
      sml_namespace::Module::Structure => match other.env.str_env.get(other_name) {
        Some(env) => {
          self.env.str_env.insert(name, env.clone());
          true
        }
        None => false,
      },
      sml_namespace::Module::Signature => match other.sig_env.get(other_name) {
        Some(env) => {
          self.sig_env.insert(name, env.clone());
          true
        }
        None => false,
      },
      sml_namespace::Module::Functor => match other.fun_env.get(other_name) {
        Some(env) => {
          self.fun_env.insert(name, env.clone());
          true
        }
        None => false,
      },
    }
  }

  /// Disallow a value.
  pub fn disallow_val(&mut self, val: &sml_path::Path) -> Option<()> {
    let env = get_mut_env(&mut self.env, val.prefix())?;
    let val_info = env.val_env.get_mut(val.last())?;
    val_info.disallow = Some(Disallow::Directly);
    Some(())
  }
}

fn get_mut_env<'e, 'n, I>(mut env: &'e mut Env, names: I) -> Option<&'e mut Env>
where
  I: IntoIterator<Item = &'n str_util::Name>,
{
  for name in names {
    env = env.str_env.get_mut(name)?;
  }
  Some(env)
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
pub fn minimal() -> (Syms, Bs) {
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
      disallow: None,
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
      disallow: None,
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
        disallow: None,
      };
      (str_util::Name::new(name.as_str()), ti)
    }))
    .collect();
  let fns = {
    let num_pair_to_num =
      overloaded(overload::Composite::Num.into(), |a| Ty::fun(dup(a.clone()), a));
    let real_pair_to_real =
      overloaded(overload::Basic::Real.into(), |a| Ty::fun(dup(a.clone()), a));
    let numtxt_pair_to_bool =
      overloaded(overload::Composite::NumTxt.into(), |a| Ty::fun(dup(a), Ty::BOOL));
    let realint_to_realint =
      overloaded(overload::Composite::RealInt.into(), |a| Ty::fun(a.clone(), a));
    let wordint_pair_to_wordint =
      overloaded(overload::Composite::WordInt.into(), |a| Ty::fun(dup(a.clone()), a));
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
    .iter()
    .flat_map(|(_, ti)| ti.val_env.iter().map(|(a, b)| (a.clone(), b.clone())))
    .chain(fns.into_iter().map(|(name, ty_scheme)| {
      let vi = ValInfo {
        ty_scheme,
        id_status: IdStatus::Val,
        defs: fast_hash::set([name.into()]),
        disallow: None,
      };
      (str_util::Name::new(name.as_str()), vi)
    }))
    .collect();
  let bs = Bs {
    fun_env: FunEnv::default(),
    sig_env: SigEnv::default(),
    env: Env { str_env: StrEnv::default(), ty_env, val_env, def: None, disallow: None },
  };
  (syms, bs)
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
    syms.start(sml_path::Path::one(str_util::Name::new(sym.primitive().unwrap().as_str())));
  assert_eq!(sym, started.sym());
  syms.finish(started, ty_info, equality);
}

fn basic_datatype(sym: Sym, ctors: &'static [PrimitiveKind]) -> TyInfo {
  let ty_scheme = TyScheme::zero(Ty::zero(sym));
  let val_env = datatype_ve(ctors.iter().map(|&x| (x, ty_scheme.clone())));
  TyInfo { ty_scheme, val_env, def: Some(sym.primitive().unwrap().into()), disallow: None }
}

fn datatype_ve<I>(xs: I) -> ValEnv
where
  I: IntoIterator<Item = (PrimitiveKind, TyScheme)>,
{
  xs.into_iter()
    .map(|(name, ty_scheme)| {
      let vi = ValInfo {
        ty_scheme,
        id_status: IdStatus::Con,
        defs: fast_hash::set([name.into()]),
        disallow: None,
      };
      (str_util::Name::new(name.as_str()), vi)
    })
    .collect()
}

fn overloaded<F>(ov: overload::Overload, f: F) -> TyScheme
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
