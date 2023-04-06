//! Types and type schemes.

use crate::overload;
use crate::sym::Sym;
use crate::ty_var::{bound::BoundTyVar, fixed::FixedTyVar, meta::MetaTyVar};
use fast_hash::FxHashMap;
use std::collections::BTreeMap;

/// Definition: Type
#[derive(Debug, Clone)]
pub(crate) enum Ty {
  None,
  /// Can only appear when this Ty is wrapped in a TyScheme.
  BoundVar(BoundTyVar),
  MetaVar(MetaTyVar),
  FixedVar(FixedTyVar),
  /// Definition: RowType
  Record(RecordTy),
  /// Definition: ConsType
  ///
  /// Use `Ty::zero` if constructing a zero-argument `Con`.
  Con(Vec<Ty>, Sym),
  /// Definition: FunType
  ///
  /// Use `Ty::fun` if constructing a `Fn` from unboxed types.
  Fn(Box<Ty>, Box<Ty>),
}

impl Ty {
  /// Returns a [`Self::Con`] with 0 arguments and the given `sym`.
  pub(crate) const fn zero(sym: Sym) -> Self {
    Self::Con(Vec::new(), sym)
  }

  /// Returns a [`Self::Fn`] from `param` to `res`.
  pub(crate) fn fun(param: Self, res: Self) -> Self {
    Self::Fn(param.into(), res.into())
  }

  pub(crate) fn desc(&self) -> &'static str {
    match self {
      Ty::None => "an unknown type",
      Ty::BoundVar(_) => "a bound type variable",
      Ty::MetaVar(_) => "an unsolved type variable",
      Ty::FixedVar(_) => "a fixed type variable",
      Ty::Record(_) => "a record or tuple type",
      Ty::Con(_, _) => "a constructor type",
      Ty::Fn(_, _) => "a function type",
    }
  }
}

pub(crate) type RecordTy = BTreeMap<sml_hir::Lab, Ty>;

/// Definition: `TypeScheme`, `TypeFcn`
#[derive(Debug, Clone)]
pub(crate) struct TyScheme {
  pub(crate) bound_vars: BoundTyVars,
  pub(crate) ty: Ty,
}

impl TyScheme {
  /// zero as in this type scheme binds zero variables.
  pub(crate) fn zero(ty: Ty) -> Self {
    Self { bound_vars: BoundTyVars::default(), ty }
  }

  /// one as in this type scheme binds one variable.
  pub(crate) fn one<F>(f: F) -> Self
  where
    F: FnOnce(Ty) -> (Ty, Option<TyVarKind>),
  {
    let mut bound_vars = BoundTyVars::new();
    let mut ty = None::<Ty>;
    BoundTyVar::add_to_binder(&mut bound_vars, |x| {
      let res = f(Ty::BoundVar(x));
      ty = Some(res.0);
      res.1
    });
    Self { bound_vars, ty: ty.unwrap() }
  }

  pub(crate) fn n_ary<I>(iter: I, sym: Sym) -> Self
  where
    I: Iterator<Item = Option<TyVarKind>>,
  {
    let bound_vars: BoundTyVars = iter.collect();
    let ty =
      Ty::Con(BoundTyVar::iter_for(bound_vars.iter()).map(|(x, _)| Ty::BoundVar(x)).collect(), sym);
    Self { bound_vars, ty }
  }
}

#[derive(Debug, Clone)]
pub(crate) enum TyVarKind {
  Equality,
  Overloaded(overload::Overload),
  /// The `Idx` is just for better error reporting.
  Record(RecordTy, sml_hir::Idx),
}

/// Information about meta type variables.
#[derive(Debug, Default, Clone)]
pub struct MetaVarInfo(FxHashMap<MetaTyVar, TyVarKind>);

impl MetaVarInfo {
  pub(crate) fn get(&self, mv: MetaTyVar) -> Option<&TyVarKind> {
    self.0.get(&mv)
  }
}

#[derive(Debug, Default)]
pub(crate) struct Subst {
  mv_info: MetaVarInfo,
  entries: FxHashMap<MetaTyVar, SubstEntry>,
}

impl Subst {
  pub(crate) fn insert(&mut self, mv: MetaTyVar, entry: SubstEntry) -> Option<SubstEntry> {
    match &entry {
      SubstEntry::Solved(_) => {}
      SubstEntry::Kind(kind) => {
        self.mv_info.0.insert(mv, kind.clone());
      }
    }
    self.entries.insert(mv, entry)
  }

  pub(crate) fn get(&self, mv: MetaTyVar) -> Option<&SubstEntry> {
    self.entries.get(&mv)
  }

  pub(crate) fn into_meta_var_info(self) -> MetaVarInfo {
    self.mv_info
  }
}

#[derive(Debug, Clone)]
pub(crate) enum SubstEntry {
  /// We solved this meta var to a `Ty`.
  Solved(Ty),
  /// This meta var is a special type variable, into which only certain kinds of types can be
  /// substituted.
  Kind(TyVarKind),
}

/// Used to be a newtype, but we ended up wanting to use many fundamental vec operations.
pub(crate) type BoundTyVars = Vec<Option<TyVarKind>>;

/// Used to be a newtype, but we ended up wanting to use many fundamental map operations.
pub(crate) type FixedTyVars = BTreeMap<FixedTyVar, Option<BoundTyVar>>;
