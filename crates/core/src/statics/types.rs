//! Types used in static analysis, many essentially as described in the Definition.
//!
//! Note that in many places we use BTreeMap and not HashMap. Though HashMap might be faster
//! (haven't actually checked), we use BTreeMap in situations where we need to sort by keys, e.g. to
//! guarantee a stable iteration order. As an example, see enrich.rs (or don't if you just came from
//! the comment there telling you to come here).

use crate::ast::{Label, TyPrec};
use crate::intern::{StrRef, StrStore};
use crate::loc::{Loc, Located};
use crate::token::TyVar as AstTyVar;
use crate::util::eq_iter;
use maplit::{btreemap, btreeset, hashmap, hashset};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt;

/// An error encountered during static analysis.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum Error {
  Undefined(Item, StrRef),
  Redefined(StrRef),
  DuplicateLabel(Label),
  Circularity(TyVar, Ty),
  TyMismatch(Ty, Ty),
  OverloadTyMismatch(Vec<Sym>, Ty),
  PatWrongIdStatus,
  ExnWrongIdStatus(IdStatus),
  WrongNumTyArgs(usize, usize),
  NonVarInAs(StrRef),
  ForbiddenBinding(StrRef),
  TyNameEscape,
  NonExhaustiveMatch,
  NonExhaustiveBinding,
  UnreachablePattern,
  FunDecNameMismatch(StrRef, StrRef),
  FunDecWrongNumPats(usize, usize),
  PatNotConsTy(Ty),
  PatNotArrowTy(Ty),
  DatatypeCopyNotDatatype,
  NotEquality(Ty),
  NotArrowTy(Ty),
  Todo(&'static str),
}

impl Error {
  /// A human-readable description of the error.
  pub fn message(&self, store: &StrStore) -> String {
    match self {
      Self::Undefined(item, id) => format!("undefined {} identifier: {}", item, store.get(*id)),
      Self::Redefined(id) => format!("redefined identifier: {}", store.get(*id)),
      Self::DuplicateLabel(lab) => format!("duplicate label: {}", show_lab(store, *lab)),
      Self::Circularity(ty_var, ty) => {
        format!("circularity: {:?} in {}", ty_var, show_ty(store, &ty))
      }
      Self::TyMismatch(want, got) => format!(
        "mismatched types: expected {}, found {}",
        show_ty(store, &want),
        show_ty(store, &got)
      ),
      Self::OverloadTyMismatch(want, got) => {
        let mut ret = "mismatched types: expected one of ".to_owned();
        for &sym in want {
          show_ty_impl(&mut ret, store, &Ty::base(sym), TyPrec::Arrow);
          ret.push_str(", ");
        }
        ret.push_str("found ");
        show_ty_impl(&mut ret, store, got, TyPrec::Arrow);
        ret
      }
      Self::PatWrongIdStatus => {
        "mismatched identifier status: expected a constructor or exception, found a value"
          .to_owned()
      }
      Self::ExnWrongIdStatus(x) => format!(
        "mismatched identifier status: expected an exception, found a {}",
        x
      ),
      Self::WrongNumTyArgs(want, got) => format!(
        "wrong number of type arguments: expected {}, found {}",
        want, got
      ),
      Self::NonVarInAs(id) => format!(
        "pattern to left of `as` is not a variable: {}",
        store.get(*id)
      ),
      Self::ForbiddenBinding(id) => format!("forbidden identifier in binding: {}", store.get(*id)),
      Self::TyNameEscape => "expression causes a type name to escape its scope".to_owned(),
      Self::NonExhaustiveMatch => "non-exhaustive match".to_owned(),
      Self::NonExhaustiveBinding => "non-exhaustive binding".to_owned(),
      Self::UnreachablePattern => "unreachable pattern".to_owned(),
      Self::FunDecNameMismatch(want, got) => format!(
        "name mismatch in function declaration: expected {}, found {}",
        store.get(*want),
        store.get(*got)
      ),
      Self::FunDecWrongNumPats(want, got) => format!(
        "wrong number of patterns in function declaration: expected {}, found {}",
        want, got
      ),
      Self::PatNotConsTy(ty) => format!(
        "pattern does not have a constructor type: {}",
        show_ty(store, ty)
      ),
      Self::PatNotArrowTy(ty) => format!(
        "pattern does not have an arrow type: {}",
        show_ty(store, ty)
      ),
      Self::DatatypeCopyNotDatatype => {
        "right-hand side of datatype copy is not a datatype".to_owned()
      }
      Self::NotEquality(ty) => format!("not an equality type: {}", show_ty(store, ty)),
      Self::NotArrowTy(ty) => format!("not a function type: {}", show_ty(store, ty)),
      Self::Todo(msg) => format!("unsupported language construct: {}", msg),
    }
  }
}

/// Show a label.
fn show_lab(store: &StrStore, lab: Label) -> String {
  match lab {
    Label::Vid(id) => store.get(id).to_owned(),
    Label::Num(n) => format!("{}", n),
  }
}

/// Show a type.
fn show_ty(store: &StrStore, ty: &Ty) -> String {
  let mut buf = String::new();
  show_ty_impl(&mut buf, store, ty, TyPrec::Arrow);
  buf
}

/// The impl of `show_ty`. This has a `TyPrec` argument to correctly show types with minimal amounts
/// of parentheses while still being correct. It also mutates the input `buf` instead of returning a
/// new `String`.
fn show_ty_impl(buf: &mut String, store: &StrStore, ty: &Ty, prec: TyPrec) {
  match ty {
    Ty::Var(tv) => buf.push_str(&format!("{:?}", tv)),
    Ty::Record(rows) => {
      if rows.is_empty() {
        buf.push_str("unit");
        return;
      }
      let is_tuple = rows.len() >= 2
        && rows
          .keys()
          .enumerate()
          .all(|(idx, lab)| Label::tuple(idx) == *lab);
      if is_tuple {
        if prec > TyPrec::Star {
          buf.push_str("(");
        }
        let mut tys = rows.values();
        let ty = tys.next().unwrap();
        show_ty_impl(buf, store, ty, TyPrec::App);
        for ty in tys {
          buf.push_str(" * ");
          show_ty_impl(buf, store, ty, TyPrec::App);
        }
        if prec > TyPrec::Star {
          buf.push_str(")");
        }
      } else {
        buf.push_str("{ ");
        let mut rows = rows.iter();
        let (lab, ty) = rows.next().unwrap();
        show_row(buf, store, *lab, ty);
        for (lab, ty) in rows {
          buf.push_str(", ");
          show_row(buf, store, *lab, ty);
        }
        buf.push_str(" }");
      }
    }
    Ty::Arrow(lhs, rhs) => {
      if prec > TyPrec::Arrow {
        buf.push_str("(");
      }
      show_ty_impl(buf, store, lhs, TyPrec::Star);
      buf.push_str(" -> ");
      show_ty_impl(buf, store, rhs, TyPrec::Arrow);
      if prec > TyPrec::Arrow {
        buf.push_str(")");
      }
    }
    Ty::Ctor(args, sym) => {
      let mut args_iter = args.iter();
      if let Some(arg) = args_iter.next() {
        if args.len() == 1 {
          show_ty_impl(buf, store, arg, TyPrec::App);
        } else {
          buf.push_str("(");
          show_ty_impl(buf, store, arg, TyPrec::Arrow);
          for arg in args_iter {
            buf.push_str(", ");
            show_ty_impl(buf, store, arg, TyPrec::Arrow);
          }
          buf.push_str(")");
        }
        buf.push_str(" ");
      }
      buf.push_str(store.get(sym.name));
    }
  }
}

/// Show a row.
fn show_row(buf: &mut String, store: &StrStore, lab: Label, ty: &Ty) {
  buf.push_str(&show_lab(store, lab));
  buf.push_str(" : ");
  show_ty_impl(buf, store, ty, TyPrec::Arrow);
}

/// A specialized Result type that many functions doing static analysis return.
pub type Result<T> = std::result::Result<T, Located<Error>>;

/// An item. Used in error messages when something wasn't defined.
#[derive(Debug)]
pub enum Item {
  /// A value.
  Val,
  /// A type.
  Ty,
  /// A type variable.
  TyVar,
  /// A structure.
  Struct,
  /// A signature.
  Sig,
  /// A functor.
  Functor,
}

impl fmt::Display for Item {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Val => f.write_str("value"),
      Self::Ty => f.write_str("type"),
      Self::TyVar => f.write_str("type variable"),
      Self::Struct => f.write_str("structure"),
      Self::Sig => f.write_str("signature"),
      Self::Functor => f.write_str("functor"),
    }
  }
}

/// A type variable.
#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
pub struct TyVar {
  id: usize,
  /// Whether this is an equality type variable.
  pub equality: bool,
}

/// This impl gives intentionally invalid SML syntax.
impl fmt::Debug for TyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "'")?;
    if self.equality {
      write!(f, "'")?;
    }
    write!(f, "{}", self.id)
  }
}

/// A substitution, a mapping from type variables to types. The types themselves may be other type
/// variables, but for all 'output' types in the substitution, there exists no type variable in any
/// 'output' type which is already mapped to something else in this substitution.
#[derive(Debug, Clone, Default)]
pub struct Subst {
  /// The conventional substitutions.
  regular: HashMap<TyVar, Ty>,
  /// The overload constraints.
  overload: HashMap<TyVar, Vec<Sym>>,
  /// Used for user-annotated type variables which may not be substituted for arbitrary types.
  bound: HashSet<TyVar>,
}

impl Subst {
  /// Mark a type variable as bound. This type variable will not be allowed to be substituted for
  /// anything in this `Subst` until `remove_bound` is called.
  pub fn insert_bound(&mut self, tv: TyVar) {
    assert!(!self.regular.contains_key(&tv));
    assert!(!self.overload.contains_key(&tv));
    assert!(self.bound.insert(tv));
  }

  /// Un-mark a type variable as bound. This will allow the type variable to be substituted in this
  /// `Subst` as normal.
  pub fn remove_bound(&mut self, tv: &TyVar) {
    assert!(!self.regular.contains_key(tv));
    assert!(!self.overload.contains_key(tv));
    assert!(self.bound.remove(tv));
  }

  /// Returns whether the type variable is bound in this `Subst`.
  pub fn is_bound(&self, tv: &TyVar) -> bool {
    self.bound.contains(tv)
  }

  /// Inserts an overloaded ty var. It will only be allowed to be one of the given base types whose
  /// symbol is given by `Sym`. The first `Sym` in the `Vec` is the symbol of the default type, used
  /// if the overloaded ty var is never constrained.
  pub fn insert_overloaded(&mut self, tv: TyVar, syms: Vec<Sym>) {
    assert!(!syms.is_empty());
    assert!(!self.bound.contains(&tv));
    assert!(!self.regular.contains_key(&tv));
    assert!(self.overload.insert(tv, syms).is_none());
  }

  /// Solve all overloaded ty vars which have not already be solved to be their default types.
  pub fn use_overloaded_defaults(&mut self) {
    let overload = std::mem::take(&mut self.overload);
    for (tv, syms) in overload {
      let ty = Ty::base(*syms.first().unwrap());
      self.insert(tv, ty);
    }
  }

  /// Returns whether this is an overloaded ty var.
  pub fn is_overloaded(&mut self, tv: &TyVar) -> bool {
    self.overload.contains_key(&tv)
  }

  /// Insert a new `TyVar` to `Ty` mapping into this `Subst`. Updates all current mappings to have
  /// the information contained by this new mapping.
  pub fn insert(&mut self, tv: TyVar, ty: Ty) {
    assert!(!self.overload.contains_key(&tv));
    assert!(!self.bound.contains(&tv));
    let subst = Self {
      regular: hashmap![tv => ty.clone()],
      overload: hashmap![],
      bound: hashset![],
    };
    for other in self.regular.values_mut() {
      other.apply(&subst);
    }
    assert!(self.regular.insert(tv, ty).is_none());
  }

  /// Returns `Ok(())` iff want and got can unify, and updates self to explain how. The types
  /// immediately have self applied to them upon entry to this function, so no need to do it
  /// yourself before calling.
  pub fn unify(&mut self, loc: Loc, tys: &Tys, mut want: Ty, mut got: Ty) -> Result<()> {
    want.apply(self);
    got.apply(self);
    match (want, got) {
      (Ty::Var(want), Ty::Var(got)) => {
        let want_bound = self.is_bound(&want);
        let got_bound = self.is_bound(&got);
        if want == got {
          assert_eq!(want_bound, got_bound);
          Ok(())
        } else if want_bound && got_bound {
          Err(loc.wrap(Error::TyMismatch(Ty::Var(want), Ty::Var(got))))
        } else if want_bound || (!got_bound && (want.equality || self.is_overloaded(&want))) {
          assert!(!got_bound);
          self.bind(loc, tys, got, Ty::Var(want))
        } else {
          assert!(!want_bound);
          self.bind(loc, tys, want, Ty::Var(got))
        }
      }
      (Ty::Var(tv), got) => {
        if self.is_bound(&tv) {
          Err(loc.wrap(Error::TyMismatch(Ty::Var(tv), got)))
        } else {
          self.bind(loc, tys, tv, got)
        }
      }
      (want, Ty::Var(tv)) => {
        if self.is_bound(&tv) {
          Err(loc.wrap(Error::TyMismatch(want, Ty::Var(tv))))
        } else {
          self.bind(loc, tys, tv, want)
        }
      }
      (Ty::Record(rows_want), Ty::Record(mut rows_got)) => {
        if !eq_iter(rows_want.keys(), rows_got.keys()) {
          return Err(loc.wrap(Error::TyMismatch(
            Ty::Record(rows_want),
            Ty::Record(rows_got),
          )));
        }
        for (lab, want) in rows_want {
          let got = rows_got.remove(&lab).unwrap();
          self.unify(loc, tys, want, got)?;
        }
        Ok(())
      }
      (Ty::Arrow(arg_want, res_want), Ty::Arrow(arg_got, res_got)) => {
        self.unify(loc, tys, *arg_want, *arg_got)?;
        self.unify(loc, tys, *res_want, *res_got)?;
        Ok(())
      }
      (Ty::Ctor(args_want, name_want), Ty::Ctor(args_got, name_got)) => {
        if name_want != name_got {
          return Err(loc.wrap(Error::TyMismatch(
            Ty::Ctor(args_want, name_want),
            Ty::Ctor(args_got, name_got),
          )));
        }
        assert_eq!(args_want.len(), args_got.len(), "mismatched Ctor args len");
        for (want, got) in args_want.into_iter().zip(args_got) {
          self.unify(loc, tys, want, got)?;
        }
        Ok(())
      }
      (want @ Ty::Record(..), got) | (want @ Ty::Arrow(..), got) | (want @ Ty::Ctor(..), got) => {
        Err(loc.wrap(Error::TyMismatch(want, got)))
      }
    }
  }

  /// A helper for `unify`, which inserts the tv => ty mapping iff tv != ty and tv not in ty.
  /// Requires that `tv` not be bound.
  fn bind(&mut self, loc: Loc, tys: &Tys, tv: TyVar, ty: Ty) -> Result<()> {
    if ty.free_ty_vars().contains(&tv) {
      return Err(loc.wrap(Error::Circularity(tv, ty)));
    }
    // here's the single solitary reason we have to pass a `Tys` all the way down here.
    if tv.equality && !ty.is_equality(tys) {
      return Err(loc.wrap(Error::NotEquality(ty)));
    }
    if let Some(syms) = self.overload.remove(&tv) {
      let syms = match &ty {
        Ty::Var(other) => {
          self.overload.insert(*other, syms);
          None
        }
        Ty::Record(_) | Ty::Arrow(_, _) => Some(syms),
        Ty::Ctor(args, sym) => {
          if args.is_empty() && syms.iter().any(|x| x == sym) {
            None
          } else {
            Some(syms)
          }
        }
      };
      if let Some(syms) = syms {
        return Err(loc.wrap(Error::OverloadTyMismatch(syms, ty)));
      }
    }
    self.insert(tv, ty);
    Ok(())
  }
}

/// A symbol, a globally unique identifier.
///
/// If you have two `StrRef`s that are equal, they may not actually be referring to the same thing.
/// By contrast, two `Sym`s are equal iff they refer to the exact same thing.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Sym {
  name: StrRef,
  id: Option<Located<usize>>,
}

impl Sym {
  /// A helper for constructing a 'base' symbol. Loosely, 'base' = 'from the standard library'.
  const fn base(name: StrRef) -> Self {
    Self { name, id: None }
  }

  /// Returns whether this is a 'base' symbol.
  pub fn is_base(&self) -> bool {
    self.id.is_none()
  }

  /// Returns the underlying name of this Sym.
  pub fn name(&self) -> StrRef {
    self.name
  }

  pub const CHAR: Self = Self::base(StrRef::CHAR);
  pub const EXN: Self = Self::base(StrRef::EXN);
  pub const BOOL: Self = Self::base(StrRef::BOOL);
  pub const STRING: Self = Self::base(StrRef::STRING);
  pub const WORD: Self = Self::base(StrRef::WORD);
  pub const INT: Self = Self::base(StrRef::INT);
  pub const REAL: Self = Self::base(StrRef::REAL);
  pub const ORDER: Self = Self::base(StrRef::ORDER);
  pub const LIST: Self = Self::base(StrRef::LIST);
  pub const REF: Self = Self::base(StrRef::REF);
  pub const UNIT: Self = Self::base(StrRef::UNIT);
}

/// A type, for the purposes of static analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
  /// TyVar
  Var(TyVar),
  /// RowType. Tuples are just records. TODO "row polymorphism" (rest patterns, etc)?
  Record(BTreeMap<Label, Ty>),
  /// FunType
  Arrow(Box<Ty>, Box<Ty>),
  /// ConsType
  Ctor(Vec<Ty>, Sym),
}

impl Ty {
  /// A helper for constructing a 'base' type.
  const fn base(sym: Sym) -> Self {
    Self::Ctor(Vec::new(), sym)
  }

  /// Given `t`, returns `t list`.
  pub fn list(elem: Self) -> Self {
    Self::Ctor(vec![elem], Sym::base(StrRef::LIST))
  }

  /// Given `t`, returns `t ref`.
  pub fn ref_(elem: Self) -> Self {
    Self::Ctor(vec![elem], Sym::base(StrRef::REF))
  }

  /// Given `t` and `u`, returns `t * u`.
  pub fn pair(lhs: Self, rhs: Self) -> Self {
    Self::Record(btreemap![Label::Num(1) => lhs, Label::Num(2) => rhs])
  }

  /// Returns the type names in this.
  pub fn ty_names(&self) -> TyNameSet {
    match self {
      Self::Var(_) => TyNameSet::new(),
      Self::Record(rows) => rows.values().flat_map(Self::ty_names).collect(),
      Self::Arrow(arg, res) => arg.ty_names().into_iter().chain(res.ty_names()).collect(),
      Self::Ctor(args, sym) => std::iter::once(*sym)
        .chain(args.iter().flat_map(Self::ty_names))
        .collect(),
    }
  }

  /// Applies a substitution to this.
  pub fn apply(&mut self, subst: &Subst) {
    match self {
      Self::Var(tv) => match subst.regular.get(tv) {
        None => {}
        Some(ty) => *self = ty.clone(),
      },
      Self::Record(rows) => {
        for ty in rows.values_mut() {
          ty.apply(subst);
        }
      }
      Self::Arrow(lhs, rhs) => {
        lhs.apply(subst);
        rhs.apply(subst);
      }
      Self::Ctor(args, _) => {
        for arg in args {
          arg.apply(subst);
        }
      }
    }
  }

  /// Returns the free type variables in this.
  pub fn free_ty_vars(&self) -> TyVarSet {
    match self {
      Self::Var(tv) => btreeset![*tv],
      Self::Record(rows) => rows.values().flat_map(Self::free_ty_vars).collect(),
      Self::Arrow(lhs, rhs) => lhs
        .free_ty_vars()
        .union(&rhs.free_ty_vars())
        .copied()
        .collect(),
      Self::Ctor(args, _) => args.iter().flat_map(Self::free_ty_vars).collect(),
    }
  }

  /// Returns whether this is an equality type.
  pub fn is_equality(&self, tys: &Tys) -> bool {
    match self {
      Self::Var(tv) => tv.equality,
      Self::Record(rows) => rows.values().all(|ty| ty.is_equality(tys)),
      Self::Arrow(_, _) => false,
      Self::Ctor(args, sym) => {
        *sym == Sym::base(StrRef::REF)
          || tys.get(sym).unwrap().equality && args.iter().all(|ty| ty.is_equality(tys))
      }
    }
  }

  pub const CHAR: Self = Self::base(Sym::CHAR);
  pub const EXN: Self = Self::base(Sym::EXN);
  pub const BOOL: Self = Self::base(Sym::BOOL);
  pub const STRING: Self = Self::base(Sym::STRING);
  pub const WORD: Self = Self::base(Sym::WORD);
  pub const INT: Self = Self::base(Sym::INT);
  pub const REAL: Self = Self::base(Sym::REAL);
  pub const ORDER: Self = Self::base(Sym::ORDER);
}

/// A type scheme, a 'forall' type.
#[derive(Debug, Clone)]
pub struct TyScheme {
  pub ty_vars: Vec<TyVar>,
  pub ty: Ty,
  /// See `instantiate` and `Subst`.
  pub overload: Option<Vec<Sym>>,
}

impl TyScheme {
  /// Given `ty`, returns `forall (). ty`, i.e. no type variables are bound by this forall.
  pub fn mono(ty: Ty) -> Self {
    Self {
      ty_vars: Vec::new(),
      ty,
      overload: None,
    }
  }

  /// Applies a substitution to this.
  pub fn apply(&mut self, subst: &Subst) {
    if self.ty_vars.iter().any(|tv| subst.regular.contains_key(tv)) {
      let mut subst = subst.clone();
      for tv in self.ty_vars.iter() {
        subst.regular.remove(tv);
      }
      self.ty.apply(&subst)
    } else {
      self.ty.apply(subst);
    }
  }

  /// Returns the free type variables in this.
  pub fn free_ty_vars(&self) -> TyVarSet {
    self
      .ty
      .free_ty_vars()
      .difference(&self.ty_vars.iter().copied().collect())
      .copied()
      .collect()
  }

  /// Given that `self` is `forall v1, ..., vn . t` and `args` is `t1, ..., tn`, returns `[t1/v1]
  /// ... [tn/vn] t`, i.e. substitutes all the argument types for the parameter type variables.
  pub fn apply_args(&self, args: Vec<Ty>) -> Ty {
    assert_eq!(args.len(), self.ty_vars.len());
    let mut subst = Subst::default();
    for (&tv, ty) in self.ty_vars.iter().zip(args) {
      subst.insert(tv, ty);
    }
    let mut ty = self.ty.clone();
    ty.apply(&subst);
    ty
  }
}

/// NOTE These are defined in exactly the same way in the Definition. Is it worth even having the
/// type alias here?
pub type TyFcn = TyScheme;

/// Information about a type that 'has been generated', like a datatype or a `type t` in a
/// signature. TyStr from the Definition.
pub struct TyInfo {
  pub ty_fcn: TyFcn,
  /// NOTE I think this is empty iff this is a special type (int, word, etc) or a `type t` in a
  /// signature. That is, this is empty iff you _cannot_ datatype copy this symbol.
  pub val_env: ValEnv,
  /// Not strictly in the Definition, but seems to be implicitly mentioned when talking about type
  /// structures respecting equality. Since a `SymTyInfo` is immutable, we compute when creating a
  /// new `SymTyInfo` whether it respects equality and cache that value in here. TODO what is this
  /// was an alias?
  pub equality: bool,
}

/// A collection of symbol types.
pub type Tys = HashMap<Sym, TyInfo>;

/// A structure environment.
pub type StrEnv = BTreeMap<StrRef, Env>;

/// A type environment.
#[derive(Clone, Default)]
pub struct TyEnv {
  pub inner: BTreeMap<StrRef, Sym>,
}

impl TyEnv {
  /// Applies a substitution to this.
  pub fn apply(&mut self, subst: &Subst, tys: &mut Tys) {
    for sym in self.inner.values() {
      tys.get_mut(sym).unwrap().ty_fcn.apply(subst);
    }
  }

  /// Returns the free type variables in this.
  pub fn free_ty_vars(&self, tys: &Tys) -> TyVarSet {
    self
      .inner
      .values()
      .flat_map(|sym| tys.get(sym).unwrap().ty_fcn.free_ty_vars())
      .collect()
  }
}

/// An identifier status description.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IdStatus {
  /// A constructor, `c`.
  Ctor,
  /// An exception, `e`.
  Exn,
  /// A value, `v`.
  Val,
}

impl IdStatus {
  /// Returns whether this `IdStatus` is `Val`.
  pub fn is_val(&self) -> bool {
    matches!(self, Self::Val)
  }

  /// Returns whether this `IdStatus` is `Exn`.
  pub fn is_exn(&self) -> bool {
    matches!(self, Self::Exn)
  }
}

impl fmt::Display for IdStatus {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Ctor => write!(f, "constructor"),
      Self::Exn => write!(f, "exception"),
      Self::Val => write!(f, "value"),
    }
  }
}

/// Information about a value.
#[derive(Clone)]
pub struct ValInfo {
  /// Its type scheme.
  pub ty_scheme: TyScheme,
  /// Its identifier status.
  pub id_status: IdStatus,
}

impl ValInfo {
  /// Returns a new `ValInfo` with the given `TyScheme` and constructor identifier status.
  pub fn ctor(ty_scheme: TyScheme) -> Self {
    Self {
      ty_scheme,
      id_status: IdStatus::Ctor,
    }
  }

  /// Returns a new `ValInfo` with the type scheme `exn` and exception identifier status.
  pub fn exn() -> Self {
    Self {
      ty_scheme: TyScheme::mono(Ty::EXN),
      id_status: IdStatus::Exn,
    }
  }

  /// Returns a new `ValInfo` with the type scheme `ty -> exn` and exception identifier status.
  pub fn exn_fn(ty: Ty) -> Self {
    // note that `TyScheme::mono` means there is a lack of generalization here, since in `exception
    // Foo of 'a` we have `Foo: t -> exn` for some _fixed_ t, not `Foo: forall t. t -> exn`.
    Self {
      ty_scheme: TyScheme::mono(Ty::Arrow(ty.into(), Ty::EXN.into())),
      id_status: IdStatus::Exn,
    }
  }

  /// Returns a new `ValInfo` with the given `TyScheme` and value identifier status.
  pub fn val(ty_scheme: TyScheme) -> Self {
    Self {
      ty_scheme,
      id_status: IdStatus::Val,
    }
  }
}

/// An environment of values.
pub type ValEnv = BTreeMap<StrRef, ValInfo>;

/// An environment. Structures (and therefore the "top-level") and signatures are essentially
/// represented as this.
#[derive(Clone, Default)]
pub struct Env {
  /// The structures defined in this structure.
  pub str_env: StrEnv,
  /// The types defined in this structure.
  pub ty_env: TyEnv,
  /// The values defined in this structure.
  pub val_env: ValEnv,
}

impl Env {
  /// Extends an environment with another. `other` overwrites `self`.
  pub fn extend(&mut self, other: Self) {
    for (name, env) in other.str_env {
      self.str_env.insert(name, env);
    }
    for (name, sym) in other.ty_env.inner {
      self.ty_env.inner.insert(name, sym);
    }
    for (name, val_info) in other.val_env {
      self.val_env.insert(name, val_info);
    }
  }

  /// Extends an environment with another, but returns `Ok(())` iff the other environment didn't
  /// overwrite anything in this.
  pub fn maybe_extend(&mut self, other: Self, loc: Loc) -> Result<()> {
    for (name, env) in other.str_env {
      if self.str_env.insert(name, env).is_some() {
        return Err(loc.wrap(Error::Redefined(name)));
      }
    }
    for (name, sym) in other.ty_env.inner {
      if self.ty_env.inner.insert(name, sym).is_some() {
        return Err(loc.wrap(Error::Redefined(name)));
      }
    }
    for (name, val_info) in other.val_env {
      if self.val_env.insert(name, val_info).is_some() {
        return Err(loc.wrap(Error::Redefined(name)));
      }
    }
    Ok(())
  }

  /// Returns the type names in this environment.
  pub fn ty_names(&self) -> TyNameSet {
    self
      .str_env
      .values()
      .flat_map(Self::ty_names)
      .chain(self.ty_env.inner.values().copied())
      .collect()
  }

  /// Applies a substitution to this.
  pub fn apply(&mut self, subst: &Subst, tys: &mut Tys) {
    for env in self.str_env.values_mut() {
      env.apply(subst, tys);
    }
    self.ty_env.apply(subst, tys);
    for val_info in self.val_env.values_mut() {
      val_info.ty_scheme.apply(subst);
    }
  }

  /// Returns the free type variables in this.
  pub fn free_ty_vars(&self, tys: &Tys) -> TyVarSet {
    self
      .str_env
      .values()
      .flat_map(|env| env.free_ty_vars(tys))
      .chain(self.ty_env.free_ty_vars(tys))
      .chain(
        self
          .val_env
          .values()
          .flat_map(|vi| vi.ty_scheme.free_ty_vars()),
      )
      .collect()
  }
}

impl From<ValEnv> for Env {
  fn from(val_env: ValEnv) -> Self {
    Self {
      str_env: StrEnv::new(),
      ty_env: TyEnv::default(),
      val_env,
    }
  }
}

impl From<TyEnv> for Env {
  fn from(ty_env: TyEnv) -> Self {
    Self {
      str_env: StrEnv::new(),
      ty_env,
      val_env: ValEnv::new(),
    }
  }
}

impl From<StrEnv> for Env {
  fn from(str_env: StrEnv) -> Self {
    Self {
      str_env,
      ty_env: TyEnv::default(),
      val_env: ValEnv::new(),
    }
  }
}

/// A set of type names.
pub type TyNameSet = HashSet<Sym>;

/// A set of type variables. NOTE this is an ordered set purely to make errors reproducible.
pub type TyVarSet = BTreeSet<TyVar>;

/// A context.
#[derive(Clone)]
pub struct Cx {
  pub ty_names: TyNameSet,
  /// In the Definition this is a set, but here we use it as not just a set, but a mapping from AST
  /// type variables to statics type variables. Note the mapping is injective but not surjective.
  pub ty_vars: HashMap<AstTyVar<StrRef>, TyVar>,
  /// The environment.
  pub env: Env,
}

impl Cx {
  /// This is the o-plus operation defined in the Definition, which extends a context by an
  /// environment and that environment's type name set.
  pub fn o_plus(&mut self, env: Env) {
    let ty_names = env.ty_names();
    self.env.extend(env);
    self.ty_names.extend(ty_names);
  }
}

/// A signature.
#[derive(Clone)]
pub struct Sig {
  pub ty_names: TyNameSet,
  pub env: Env,
}

/// A functor signature.
#[derive(Clone)]
pub struct FunSig {
  pub ty_names: TyNameSet,
  pub env: Env,
  pub sig: Sig,
}

/// A signature environment.
pub type SigEnv = HashMap<StrRef, Sig>;

/// A functor environment.
pub type FunEnv = HashMap<StrRef, FunSig>;

/// A basis. There's one of these in the whole program, since it basically represents the entire
/// program.
#[derive(Clone)]
pub struct Basis {
  // TODO is this really necessary?
  pub ty_names: TyNameSet,
  pub fun_env: FunEnv,
  pub sig_env: SigEnv,
  pub env: Env,
}

impl Basis {
  /// Apply a substitution to this.
  pub fn apply(&mut self, subst: &Subst, tys: &mut Tys) {
    for fun_sig in self.fun_env.values_mut() {
      fun_sig.env.apply(subst, tys);
      fun_sig.sig.env.apply(subst, tys);
    }
    for sig in self.sig_env.values_mut() {
      sig.env.apply(subst, tys);
    }
    self.env.apply(subst, tys);
  }

  /// Return the free type variables in this. Should always be empty, as per the Definition.
  pub fn free_ty_vars(&self, tys: &Tys) -> TyVarSet {
    self
      .fun_env
      .values()
      .flat_map(|fun_sig| {
        fun_sig
          .env
          .free_ty_vars(tys)
          .into_iter()
          .chain(fun_sig.sig.env.free_ty_vars(tys))
      })
      .chain(
        self
          .sig_env
          .values()
          .flat_map(|sig| sig.env.free_ty_vars(tys)),
      )
      .chain(self.env.free_ty_vars(tys))
      .collect()
  }

  /// Returns a context derived from the information in this.
  pub fn to_cx(&self) -> Cx {
    Cx {
      ty_names: self.ty_names.clone(),
      ty_vars: HashMap::new(),
      env: self.env.clone(),
    }
  }

  /// Add an environment to this.
  pub fn add_env(&mut self, env: Env) {
    let ty_names = env.ty_names();
    self.ty_names.extend(ty_names);
    self.env.extend(env);
  }

  /// Add an signature environment to this.
  pub fn add_sig_env(&mut self, sig_env: SigEnv) {
    let ty_names = sig_env
      .values()
      .flat_map(|sig| sig.ty_names.iter())
      .copied();
    self.ty_names.extend(ty_names);
    self.sig_env.extend(sig_env);
  }

  /// Add an functor environment to this.
  pub fn add_fun_env(&mut self, fun_env: FunEnv) {
    let ty_names = fun_env
      .values()
      .flat_map(|sig| sig.ty_names.iter())
      .copied();
    self.ty_names.extend(ty_names);
    self.fun_env.extend(fun_env);
  }
}

/// The state passed around by many of the statics functions. There's only one of these, and it's
/// constantly being mutably, additively updated as we go.
#[derive(Default)]
pub struct State {
  /// The next type variable ID to hand out. Invariant: Always increases.
  next_ty_var: usize,
  /// The next symbol ID to hand out. Invariant: Always increase.
  next_sym: usize,
  /// The substitution, the unifier of the entire program. Invariant: Always grows in size.
  pub subst: Subst,
  /// The types that 'have been generated' and information about them. Invariant: Always grows in
  /// size.
  pub tys: Tys,
}

impl State {
  /// Returns a fresh type variable.
  pub fn new_ty_var(&mut self, equality: bool) -> TyVar {
    let id = self.next_ty_var;
    self.next_ty_var += 1;
    TyVar { id, equality }
  }

  /// Returns a fresh symbol.
  pub fn new_sym(&mut self, name: Located<StrRef>) -> Sym {
    let id = Some(name.loc.wrap(self.next_sym));
    self.next_sym += 1;
    Sym { id, name: name.val }
  }

  /// A thin wrapper over `Subst#unify`, which passes in this `State`'s `Tys`.
  pub fn unify(&mut self, loc: Loc, want: Ty, got: Ty) -> Result<()> {
    self.subst.unify(loc, &self.tys, want, got)
  }
}

/// A pattern, for the purposes of static analysis. See exhaustive.rs.
#[derive(Debug, Clone)]
pub enum Pat {
  /// Matches anything. Used for variables and wildcards.
  Anything,
  /// Matches a constructor with the given arguments.
  Con(Con, Vec<Pat>),
}

impl Pat {
  /// Returns a constructor pattern with zero arguments.
  pub fn zero(con: Con) -> Self {
    Self::Con(con, vec![])
  }

  /// Returns a record pattern. Requires the patterns be sorted by label.
  pub fn record(mut pats: Vec<Pat>) -> Self {
    if pats.len() == 1 {
      // may happen in the desugaring of `Fun`.
      pats.pop().unwrap()
    } else {
      Self::Con(Con::Record(pats.len()), pats)
    }
  }
}

/// A constructor for a pattern. It is a bit confusing to have both 'Con' and 'Ctor'. We originally
/// used 'Ctor' to mean 'constructor', and then adopted 'Con' as well from reading the paper which
/// was the basis of the exhaustiveness checker.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Con {
  Int(i32),
  Word(i32),
  String(StrRef),
  Char(u8),
  /// This should never be used directly, use `Pat::record` instead. The usize is the arity.
  Record(usize),
  /// A constructor from a `datatype` or an `exception`.
  Ctor(StrRef, Span),
}

/// A measure of how many constructors exist for a type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Span {
  Finite(usize),
  PosInf,
}

impl Con {
  /// Returns the span of this.
  pub fn span(&self) -> Span {
    match *self {
      Self::Int(_) | Self::Word(_) | Self::String(_) => Span::PosInf,
      Self::Char(_) => Span::Finite(256),
      Self::Record(_) => Span::Finite(1),
      Self::Ctor(_, s) => s,
    }
  }
}

/// The span of a Con::Char is 256 = 2^8 since a char is a u8. This test and the definition of
/// `Con#span` should change if Char ever becomes not a u8.
#[test]
fn char_span() {
  assert_eq!(Con::Char(0u8).span(), Span::Finite(256));
}
