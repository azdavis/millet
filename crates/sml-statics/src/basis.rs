//! Bases. (The plural of "basis".)

use crate::env::{Cx, Env, FunEnv, SigEnv, StrEnv};
use crate::get_env::get_mut_env;
use fast_hash::FxHashMap;
use sml_statics_types::disallow::{self, Disallow};
use sml_statics_types::info::{IdStatus, TyEnv, TyInfo, ValEnv, ValInfo};
use sml_statics_types::sym::{Equality, Sym, Syms};
use sml_statics_types::ty::{BoundTyVar, RecordData, Ty, TyScheme, TyVarKind, Tys};
use sml_statics_types::{def::Primitive, item::Item, overload};

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
  ///
  /// # Errors
  ///
  /// If the path couldn't be disallowed.
  pub fn disallow_val(&mut self, val: &sml_path::Path) -> Result<(), disallow::Error> {
    let env = match get_mut_env(&mut self.env, val.prefix()) {
      Ok(x) => x,
      Err(n) => return Err(disallow::Error::Undefined(Item::Struct, n.clone())),
    };
    let val_info = match env.val_env.get_mut(val.last()) {
      Some(x) => x,
      None => return Err(disallow::Error::Undefined(Item::Val, val.last().clone())),
    };
    match &val_info.disallow {
      None => {
        val_info.disallow = Some(Disallow::Directly);
        Ok(())
      }
      Some(x) => Err(disallow::Error::Already(x.clone())),
    }
  }

  /// Disallow a structure, transitively including all of its items.
  ///
  /// # Errors
  ///
  /// If the path couldn't be disallowed.
  pub fn disallow_str(&mut self, val: &sml_path::Path) -> Result<(), disallow::Error> {
    let env = match get_mut_env(&mut self.env, val.prefix()) {
      Ok(x) => x,
      Err(n) => return Err(disallow::Error::Undefined(Item::Struct, n.clone())),
    };
    let env = match env.str_env.get_mut(val.last()) {
      Some(x) => x,
      None => return Err(disallow::Error::Undefined(Item::Struct, val.last().clone())),
    };
    match &env.disallow {
      None => {
        env.disallow = Some(Disallow::Directly);
        Ok(())
      }
      Some(x) => Err(disallow::Error::Already(x.clone())),
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
pub fn minimal() -> (sml_statics_types::St, Bs) {
  let mut tys = Tys::default();
  // @sync(special_sym_order)
  let mut syms = Syms::default();
  for sym in [Sym::INT, Sym::WORD, Sym::REAL, Sym::CHAR, Sym::STRING] {
    insert_special(&mut syms, sym, basic_datatype(&mut tys, sym, &[]));
  }
  syms.overloads_mut().int.push(Sym::INT);
  syms.overloads_mut().word.push(Sym::WORD);
  syms.overloads_mut().real.push(Sym::REAL);
  syms.overloads_mut().char.push(Sym::CHAR);
  syms.overloads_mut().string.push(Sym::STRING);
  let bool_info = basic_datatype(&mut tys, Sym::BOOL, &[Primitive::True, Primitive::False]);
  insert_special(&mut syms, Sym::BOOL, bool_info);
  let list_info = {
    let list = |tys: &mut Tys, a: Ty| tys.con(vec![a], Sym::LIST);
    let alpha_list = ty_scheme_one(&mut tys, TyVarKind::Regular, list);
    let cons = ty_scheme_one(&mut tys, TyVarKind::Regular, |tys, a| {
      let a_list = list(tys, a);
      let pair_a_a_list = pair(tys, a, a_list);
      tys.fun(pair_a_a_list, a_list)
    });
    TyInfo {
      ty_scheme: alpha_list.clone(),
      val_env: datatype_ve([(Primitive::Nil, alpha_list), (Primitive::Cons, cons)]),
      def: Some(Primitive::List.into()),
      disallow: None,
    }
  };
  insert_special(&mut syms, Sym::LIST, list_info);
  let ref_info = {
    let ref_ = |tys: &mut Tys, a: Ty| tys.con(vec![a], Sym::REF);
    let con = ty_scheme_one(&mut tys, TyVarKind::Regular, |tys, a| {
      let a_ref = ref_(tys, a);
      tys.fun(a, a_ref)
    });
    TyInfo {
      ty_scheme: ty_scheme_one(&mut tys, TyVarKind::Regular, ref_),
      val_env: datatype_ve([(Primitive::RefVal, con)]),
      def: Some(Primitive::RefTy.into()),
      disallow: None,
    }
  };
  insert_special(&mut syms, Sym::REF, ref_info);
  let aliases = [(Primitive::Unit, Ty::UNIT), (Primitive::Exn, Ty::EXN)];
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
      ov_fn(&mut tys, overload::Composite::Num.into(), |tys, a| (dup(tys, a), a));
    let real_pair_to_real =
      ov_fn(&mut tys, overload::Basic::Real.into(), |tys, a| (dup(tys, a), a));
    let numtxt_pair_to_bool =
      ov_fn(&mut tys, overload::Composite::NumTxt.into(), |tys, a| (dup(tys, a), Ty::BOOL));
    let realint_to_realint = ov_fn(&mut tys, overload::Composite::RealInt.into(), |_, a| (a, a));
    let wordint_pair_to_wordint =
      ov_fn(&mut tys, overload::Composite::WordInt.into(), |tys, a| (dup(tys, a), a));
    let equality_pair_to_bool = ty_scheme_one(&mut tys, TyVarKind::Equality, |tys, a| {
      let a_a = dup(tys, a);
      tys.fun(a_a, Ty::BOOL)
    });
    [
      (Primitive::Mul, num_pair_to_num.clone()),
      (Primitive::Add, num_pair_to_num.clone()),
      (Primitive::Sub, num_pair_to_num),
      (Primitive::RealDiv, real_pair_to_real),
      (Primitive::Lt, numtxt_pair_to_bool.clone()),
      (Primitive::LtEq, numtxt_pair_to_bool.clone()),
      (Primitive::Gt, numtxt_pair_to_bool.clone()),
      (Primitive::GtEq, numtxt_pair_to_bool),
      (Primitive::Neg, realint_to_realint.clone()),
      (Primitive::Abs, realint_to_realint),
      (Primitive::Div, wordint_pair_to_wordint.clone()),
      (Primitive::Mod, wordint_pair_to_wordint),
      (Primitive::Eq, equality_pair_to_bool.clone()),
      (Primitive::Neq, equality_pair_to_bool),
      (Primitive::Use, TyScheme::zero(tys.fun(Ty::STRING, Ty::UNIT))),
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
  (sml_statics_types::St { syms, tys }, bs)
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

fn basic_datatype(tys: &mut Tys, sym: Sym, ctors: &'static [Primitive]) -> TyInfo {
  let ty_scheme = TyScheme::zero(tys.con(Vec::new(), sym));
  let val_env = datatype_ve(ctors.iter().map(|&x| (x, ty_scheme.clone())));
  TyInfo { ty_scheme, val_env, def: Some(sym.primitive().unwrap().into()), disallow: None }
}

fn datatype_ve<I>(xs: I) -> ValEnv
where
  I: IntoIterator<Item = (Primitive, TyScheme)>,
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

fn dup(tys: &mut Tys, ty: Ty) -> Ty {
  pair(tys, ty, ty)
}

fn pair(tys: &mut Tys, t1: Ty, t2: Ty) -> Ty {
  tys.record(RecordData::from([(sml_hir::Lab::Num(1), t1), (sml_hir::Lab::Num(2), t2)]))
}

fn ov_fn<F>(tys: &mut Tys, ov: overload::Overload, f: F) -> TyScheme
where
  F: FnOnce(&mut Tys, Ty) -> (Ty, Ty),
{
  ty_scheme_one(tys, TyVarKind::Overloaded(ov), |tys, a| {
    let (a, b) = f(tys, a);
    tys.fun(a, b)
  })
}

fn ty_scheme_one<F>(tys: &mut Tys, k: TyVarKind, f: F) -> TyScheme
where
  F: FnOnce(&mut Tys, Ty) -> Ty,
{
  let mut bound_vars = Vec::<TyVarKind>::new();
  let mut ty = None::<Ty>;
  BoundTyVar::add_to_binder(&mut bound_vars, |bv| {
    ty = Some(Ty::bound_var(bv));
    k
  });
  TyScheme { bound_vars, ty: f(tys, ty.unwrap()) }
}
