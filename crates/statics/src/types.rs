//! Types.

use fast_hash::FxHashMap;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use uniq::{Uniq, UniqGen};

/// Definition: Type
#[derive(Debug, Clone)]
pub(crate) enum Ty {
  None,
  /// Can only appear when this Ty is wrapped in a TyScheme.
  BoundVar(BoundTyVar),
  MetaVar(MetaTyVar),
  FixedVar(FixedTyVar),
  /// Definition: RowType
  Record(BTreeMap<hir::Lab, Ty>),
  /// Definition: ConsType
  Con(Vec<Ty>, Sym),
  /// Definition: FunType
  Fn(Box<Ty>, Box<Ty>),
}

impl Ty {
  /// Returns a [`Self::Con`] with 0 arguments and the given `sym`.
  pub(crate) fn zero(sym: Sym) -> Self {
    Self::Con(Vec::new(), sym)
  }

  /// TODO do we need this? have it be on TyScheme, Ty, both?
  pub(crate) fn display<'a>(&'a self, syms: &'a Syms) -> impl fmt::Display + 'a {
    TyDisplay {
      ty: self,
      vars: None,
      syms,
      prec: TyPrec::Arrow,
    }
  }
}

struct TyDisplay<'a> {
  ty: &'a Ty,
  /// TODO figure this out
  vars: Option<&'a TyVars>,
  syms: &'a Syms,
  prec: TyPrec,
}

impl<'a> TyDisplay<'a> {
  fn with(&self, ty: &'a Ty, prec: TyPrec) -> Self {
    Self {
      ty,
      vars: self.vars,
      syms: self.syms,
      prec,
    }
  }
}

impl<'a> fmt::Display for TyDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty {
      Ty::None => f.write_str("_")?,
      Ty::BoundVar(bv) => {
        let vars = self.vars.expect("bound ty var without a TyScheme");
        f.write_str(equality_str(vars.inner[bv.0]))?;
        let alpha = (b'z' - b'a') as usize;
        let quot = bv.0 / alpha;
        let rem = bv.0 % alpha;
        let ch = char::from((rem as u8) + b'a');
        for _ in 0..=quot {
          write!(f, "{ch}")?;
        }
      }
      // not real syntax
      Ty::MetaVar(mv) => mv.fmt(f)?,
      Ty::FixedVar(fv) => fv.fmt(f)?,
      Ty::Record(rows) => {
        if rows.is_empty() {
          return f.write_str("unit");
        }
        let is_tuple = rows.len() > 1
          && rows
            .keys()
            .enumerate()
            .all(|(idx, lab)| hir::Lab::tuple(idx) == *lab);
        if is_tuple {
          let needs_parens = self.prec > TyPrec::Star;
          if needs_parens {
            f.write_str("(")?;
          }
          let mut tys = rows.values();
          let ty = tys.next().unwrap();
          self.with(ty, TyPrec::App).fmt(f)?;
          for ty in tys {
            f.write_str(" * ")?;
            self.with(ty, TyPrec::App).fmt(f)?;
          }
          if needs_parens {
            f.write_str(")")?;
          }
        } else {
          f.write_str("{ ")?;
          let mut rows = rows.iter();
          let (lab, ty) = rows.next().unwrap();
          display_row(f, self.vars, self.syms, lab, ty)?;
          for (lab, ty) in rows {
            f.write_str(", ")?;
            display_row(f, self.vars, self.syms, lab, ty)?;
          }
          f.write_str(" }")?;
        }
      }
      Ty::Con(args, sym) => {
        let mut args_iter = args.iter();
        if let Some(arg) = args_iter.next() {
          if args.len() == 1 {
            self.with(arg, TyPrec::App).fmt(f)?;
          } else {
            f.write_str("(")?;
            self.with(arg, TyPrec::Arrow).fmt(f)?;
            for arg in args_iter {
              f.write_str(", ")?;
              self.with(arg, TyPrec::Arrow).fmt(f)?;
            }
            f.write_str(")")?;
          }
          f.write_str(" ")?;
        }
        self.syms.get_name(sym).fmt(f)?
      }
      Ty::Fn(param, res) => {
        let needs_parens = self.prec > TyPrec::Arrow;
        if needs_parens {
          f.write_str("(")?;
        }
        self.with(param, TyPrec::Star).fmt(f)?;
        f.write_str(" -> ")?;
        self.with(res, TyPrec::Arrow).fmt(f)?;
        if needs_parens {
          f.write_str(")")?;
        }
      }
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  Arrow,
  Star,
  App,
}

fn display_row<'a>(
  f: &mut fmt::Formatter<'_>,
  vars: Option<&'a TyVars>,
  syms: &'a Syms,
  lab: &hir::Lab,
  ty: &'a Ty,
) -> fmt::Result {
  fmt::Display::fmt(lab, f)?;
  f.write_str(" : ")?;
  let td = TyDisplay {
    ty,
    vars,
    syms,
    prec: TyPrec::Arrow,
  };
  fmt::Display::fmt(&td, f)
}

fn equality_str(equality: bool) -> &'static str {
  if equality {
    "''"
  } else {
    "'"
  }
}

/// Definition: TypeScheme, TypeFcn
#[derive(Debug, Clone)]
pub(crate) struct TyScheme {
  pub(crate) vars: TyVars,
  pub(crate) ty: Ty,
}

impl TyScheme {
  pub(crate) fn mono(ty: Ty) -> Self {
    Self {
      vars: TyVars { inner: Vec::new() },
      ty,
    }
  }

  /// TODO do we need this? have it be on TyScheme, Ty, both?
  pub(crate) fn display<'a>(&'a self, syms: &'a Syms) -> impl fmt::Display + 'a {
    TyDisplay {
      ty: &self.ty,
      vars: Some(&self.vars),
      syms,
      prec: TyPrec::Arrow,
    }
  }

  fn zero(s: Sym) -> Self {
    Self::mono(Ty::zero(s))
  }

  fn one(s: Sym) -> Self {
    Self {
      vars: TyVars { inner: vec![false] },
      ty: Ty::Con(vec![], s),
    }
  }
}

#[derive(Debug, Clone)]
pub(crate) struct TyVars {
  /// The length gives how many ty vars are brought into scope. The ith `bool` says whether the type
  /// variable i is equality.
  inner: Vec<bool>,
}

impl TyVars {
  pub(crate) fn is_empty(&self) -> bool {
    self.inner.is_empty()
  }
}

/// Definition: TyVar
///
/// But only kind of. There's also:
///
/// - [`MetaTyVar`]
/// - [`FixedTyVar`]
/// - [`hir::TyVar`]
///
/// Basically a de Bruijn index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct BoundTyVar(usize);

impl BoundTyVar {
  pub(crate) fn index_into<'a, T>(&self, xs: &'a [T]) -> &'a T {
    xs.get(self.0).unwrap()
  }
}

/// Generated, and to be substituted for a real type, by the inference algorithm.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct MetaTyVar {
  id: Uniq,
  equality: bool,
}

impl fmt::Display for MetaTyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}{}", equality_str(self.equality), self.id)
  }
}

#[derive(Debug, Default)]
pub(crate) struct MetaTyVarGen(UniqGen);

impl MetaTyVarGen {
  pub(crate) fn gen(&mut self, equality: bool) -> MetaTyVar {
    MetaTyVar {
      id: self.0.gen(),
      equality,
    }
  }

  pub(crate) fn gen_from_ty_vars<'a>(
    &'a mut self,
    ty_vars: &'a TyVars,
  ) -> impl Iterator<Item = MetaTyVar> + 'a {
    ty_vars.inner.iter().map(|&eq| self.gen(eq))
  }
}

/// Corresponds to a user written type variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FixedTyVar {
  id: Uniq,
  ty_var: hir::TyVar,
}

impl fmt::Display for FixedTyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.ty_var.fmt(f)
  }
}

#[derive(Debug, Default)]
pub(crate) struct FixedTyVarGen(UniqGen);

impl FixedTyVarGen {
  pub(crate) fn gen(&mut self, ty_var: hir::TyVar) -> FixedTyVar {
    FixedTyVar {
      id: self.0.gen(),
      ty_var,
    }
  }
}

/// Definition: TyName
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Sym(usize);

impl Sym {
  // keep in sync with impl Default for Syms
  pub(crate) const BOOL: Self = Self(0);
  pub(crate) const CHAR: Self = Self(1);
  pub(crate) const INT: Self = Self(2);
  pub(crate) const REAL: Self = Self(3);
  pub(crate) const STRING: Self = Self(4);
  pub(crate) const WORD: Self = Self(5);
  pub(crate) const EXN: Self = Self(6);
  pub(crate) const REF: Self = Self(7);
  pub(crate) const LIST: Self = Self(8);
  pub(crate) const ORDER: Self = Self(9);

  /// Returns whether this `Sym` was generated by a [`Syms`] after that `Syms` generated the
  /// `marker`.
  pub(crate) fn generated_after(&self, marker: &SymsMarker) -> bool {
    self.0 >= marker.0
  }
}

/// A mapping from [`Sym`]s to [`TyInfo`]s.
#[derive(Debug)]
pub struct Syms {
  store: Vec<TyInfo>,
  names: Vec<hir::Name>,
}

impl Default for Syms {
  fn default() -> Self {
    let zero = TyScheme::zero;
    let one = TyScheme::one;
    let bv = Ty::BoundVar(BoundTyVar(0));
    // keep in sync with names
    let store = vec![
      datatype(zero(Sym::BOOL), [("true", None), ("false", None)]),
      datatype(zero(Sym::CHAR), []),
      datatype(zero(Sym::INT), []),
      datatype(zero(Sym::REAL), []),
      datatype(zero(Sym::STRING), []),
      datatype(zero(Sym::WORD), []),
      datatype(zero(Sym::EXN), []),
      datatype(one(Sym::REF), [("ref", Some(bv.clone()))]),
      datatype(one(Sym::LIST), [("nil", None), ("::", Some(bv))]),
      datatype(
        zero(Sym::ORDER),
        [("LESS", None), ("EQUAL", None), ("GREATER", None)],
      ),
    ];
    let names = [
      "bool", "char", "int", "real", "string", "word", "exn", "ref", "list", "order",
    ]
    .into_iter()
    .map(hir::Name::new)
    .collect();
    Self { store, names }
  }
}

fn datatype<const N: usize>(ty_scheme: TyScheme, ctors: [(&str, Option<Ty>); N]) -> TyInfo {
  let val_env: FxHashMap<_, _> = ctors
    .into_iter()
    .map(|(name, arg)| {
      let ty_scheme = match arg {
        None => ty_scheme.clone(),
        Some(arg) => TyScheme {
          vars: ty_scheme.vars.clone(),
          ty: Ty::Fn(arg.into(), ty_scheme.ty.clone().into()),
        },
      };
      let val_info = ValInfo {
        ty_scheme,
        id_status: IdStatus::Con,
      };
      (hir::Name::new(name), val_info)
    })
    .collect();
  TyInfo { ty_scheme, val_env }
}

impl Syms {
  pub(crate) fn is_empty(&self) -> bool {
    self.store.is_empty()
  }

  pub(crate) fn insert(&mut self, ty_info: TyInfo) -> Sym {
    let ret = Sym(self.store.len());
    self.store.push(ty_info);
    ret
  }

  pub(crate) fn get(&self, sym: &Sym) -> &TyInfo {
    self.store.get(sym.0).unwrap()
  }

  fn get_name(&self, sym: &Sym) -> &hir::Name {
    self.names.get(sym.0).unwrap()
  }

  pub(crate) fn mark(&self) -> SymsMarker {
    SymsMarker(self.store.len())
  }
}

pub(crate) struct SymsMarker(usize);

/// Definition: TyStr
#[derive(Debug)]
pub(crate) struct TyInfo {
  pub(crate) ty_scheme: TyScheme,
  pub(crate) val_env: ValEnv,
}

/// Definition: StrEnv
pub(crate) type StrEnv = FxHashMap<hir::Name, Env>;

/// Definition: TyEnv
pub(crate) type TyEnv = FxHashMap<hir::Name, Sym>;

/// Definition: ValEnv
pub(crate) type ValEnv = FxHashMap<hir::Name, ValInfo>;

#[derive(Debug, Clone)]
pub(crate) struct ValInfo {
  pub(crate) ty_scheme: TyScheme,
  pub(crate) id_status: IdStatus,
}

/// Definition: IdStatus
#[derive(Debug, Clone)]
pub(crate) enum IdStatus {
  Con,
  Exn,
  Val,
}

/// Definition: Env
#[derive(Debug, Default, Clone)]
pub(crate) struct Env {
  pub(crate) str_env: StrEnv,
  pub(crate) ty_env: TyEnv,
  pub(crate) val_env: ValEnv,
}

impl Env {
  pub(crate) fn extend(&mut self, env: Env) {
    self.str_env.extend(env.str_env);
    self.ty_env.extend(env.ty_env);
    self.val_env.extend(env.val_env);
  }
}

/// Definition: Context
///
/// TODO add ty names and ty vars
#[derive(Debug, Default, Clone)]
pub(crate) struct Cx {
  pub(crate) env: Env,
  pub(crate) ty_vars: FxHashMap<hir::TyVar, FixedTyVar>,
}

/// A mapping from [`MetaTyVar`]s to [`Ty`]s.
///
/// Invariant: Mappings are never removed.
#[derive(Debug, Default)]
pub(crate) struct Subst {
  map: FxHashMap<MetaTyVar, Ty>,
}

impl Subst {
  /// Panics if there was already an assignment for this [`MetaTyVar`].
  pub(crate) fn insert(&mut self, mv: MetaTyVar, ty: Ty) {
    assert!(self.map.insert(mv, ty).is_none())
  }

  pub(crate) fn get(&self, mv: &MetaTyVar) -> Option<&Ty> {
    self.map.get(mv)
  }
}

pub(crate) fn prepare_generalize(set: BTreeSet<MetaTyVar>) -> (TyVars, Subst) {
  let ty_vars = TyVars {
    inner: set.iter().map(|mv| mv.equality).collect(),
  };
  let subst = Subst {
    map: set
      .into_iter()
      .enumerate()
      .map(|(idx, mv)| (mv, Ty::BoundVar(BoundTyVar(idx))))
      .collect(),
  };
  (ty_vars, subst)
}
