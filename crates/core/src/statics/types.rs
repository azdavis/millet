//! Types used in static analysis, many essentially as described in the Definition.

use crate::ast::Label;
use crate::intern::{StrRef, StrStore};
use crate::loc::{Loc, Located};
use maplit::{btreemap, hashmap, hashset};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::fmt;

/// An error encountered during static analysis.
#[allow(missing_docs)]
pub enum Error {
  Undefined(Item, StrRef),
  Redefined(StrRef),
  DuplicateLabel(Label),
  Circularity(TyVar, Ty),
  TyMismatch(Ty, Ty),
  PatWrongIdStatus,
  ExnWrongIdStatus(IdStatus),
  WrongNumTyArgs(usize, usize),
  NonVarInAs(StrRef),
  ForbiddenBinding(StrRef),
  NoSuitableOverload,
  TyNameEscape,
  NonExhaustiveMatch,
  NonExhaustiveBinding,
  UnreachablePattern,
  FunDecNameMismatch(StrRef, StrRef),
  FunDecWrongNumPats(usize, usize),
  PatNotConsType(Ty),
  PatNotArrowType(Ty),
  DatatypeCopyNotDatatype,
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
      Self::TyMismatch(lhs, rhs) => format!(
        "mismatched types: expected {}, found {}",
        show_ty(store, &lhs),
        show_ty(store, &rhs)
      ),
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
      Self::NoSuitableOverload => "no suitable overload found".to_owned(),
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
      Self::PatNotConsType(ty) => format!(
        "pattern does not have a constructor type: {}",
        show_ty(store, ty)
      ),
      Self::PatNotArrowType(ty) => format!(
        "pattern does not have an arrow type: {}",
        show_ty(store, ty)
      ),
      Self::DatatypeCopyNotDatatype => {
        "right-hand side of datatype copy is not a datatype".to_owned()
      }
      Self::Todo(msg) => format!("unimplemented language construct: {}", msg),
    }
  }
}

fn show_lab(store: &StrStore, lab: Label) -> String {
  match lab {
    Label::Vid(id) => store.get(id).to_owned(),
    Label::Num(n) => format!("{}", n),
  }
}

fn show_ty(store: &StrStore, ty: &Ty) -> String {
  let mut buf = String::new();
  show_ty_impl(&mut buf, store, ty);
  buf
}

fn show_ty_impl(buf: &mut String, store: &StrStore, ty: &Ty) {
  match ty {
    Ty::Var(tv) => buf.push_str(&format!("{:?}", tv)),
    Ty::Record(rows) => {
      buf.push_str("{ ");
      let mut rows = rows.iter();
      if let Some((lab, ty)) = rows.next() {
        show_row(buf, store, *lab, ty);
      }
      for (lab, ty) in rows {
        buf.push_str(", ");
        show_row(buf, store, *lab, ty);
      }
      buf.push_str(" }");
    }
    Ty::Arrow(lhs, rhs) => {
      buf.push_str("(");
      show_ty_impl(buf, store, lhs);
      buf.push_str(") -> (");
      show_ty_impl(buf, store, rhs);
      buf.push_str(")");
    }
    Ty::Ctor(args, sym) => {
      if args.is_empty() {
        buf.push_str(store.get(sym.name));
        return;
      }
      buf.push_str("(");
      let mut args = args.iter();
      if let Some(arg) = args.next() {
        show_ty_impl(buf, store, arg);
      }
      for arg in args {
        buf.push_str(", ");
        show_ty_impl(buf, store, arg);
      }
      buf.push_str(") ");
      buf.push_str(store.get(sym.name));
    }
  }
}

fn show_row(buf: &mut String, store: &StrStore, lab: Label, ty: &Ty) {
  buf.push_str(&show_lab(store, lab));
  buf.push_str(" : ");
  show_ty_impl(buf, store, ty);
}

/// A specialized Result type that many functions doing static analysis return.
pub type Result<T> = std::result::Result<T, Located<Error>>;

pub enum Item {
  Value,
  Type,
  Structure,
  Signature,
  Functor,
}

impl fmt::Display for Item {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Value => write!(f, "value"),
      Self::Type => write!(f, "type"),
      Self::Structure => write!(f, "structure"),
      Self::Signature => write!(f, "signature"),
      Self::Functor => write!(f, "functor"),
    }
  }
}

/// A symbol, used to uniquely identify a type which 'has been generated'.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Sym {
  name: StrRef,
  id: Option<Located<usize>>,
}

impl Sym {
  pub const fn base(name: StrRef) -> Self {
    Self { name, id: None }
  }
}

/// A type variable.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct TyVar {
  id: usize,
  /// Whether this is an equality type variable.
  pub equality: bool,
}

impl fmt::Debug for TyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "'")?;
    if self.equality {
      write!(f, "'")?;
    }
    write!(f, "t{}", self.id)
  }
}

/// A substitution, a mapping from type variables to types. The types themselves may be other type
/// variables, but for all 'output' types in the substitution, there exists no type variable in any
/// 'output' type which is already mapped to something else in this substitution.
#[derive(Clone, Default)]
pub struct Subst {
  inner: HashMap<TyVar, Ty>,
}

impl Subst {
  /// Insert a new TyVar => Ty mapping into this Subst. Updates all current mappings to have the
  /// information contained by this new mapping. Panics if this TyVar already mapped to something.
  pub fn insert(&mut self, tv: TyVar, ty: Ty) {
    for (_, other) in self.inner.iter_mut() {
      other.apply(&Self {
        inner: hashmap![tv => ty.clone()],
      });
    }
    assert!(self.inner.insert(tv, ty).is_none());
  }

  /// a helper for unify, which inserts the tv => ty mapping iff tv != ty and tv not in ty.
  fn bind(&mut self, loc: Loc, tv: TyVar, ty: Ty) -> Result<()> {
    if let Ty::Var(other) = ty {
      if tv == other {
        return Ok(());
      }
    }
    if ty.free_ty_vars().contains(&tv) {
      return Err(loc.wrap(Error::Circularity(tv, ty)));
    }
    self.insert(tv, ty);
    Ok(())
  }

  /// want = expected, got = found. the types immediately have self applied to them upon entry to
  /// this function, so no need to do it yourself before calling.
  pub fn unify(&mut self, loc: Loc, mut want: Ty, mut got: Ty) -> Result<()> {
    want.apply(self);
    got.apply(self);
    match (want, got) {
      (Ty::Var(tv), got) => self.bind(loc, tv, got),
      (want, Ty::Var(tv)) => self.bind(loc, tv, want),
      (Ty::Record(rows_want), Ty::Record(mut rows_got)) => {
        let mut keys_want = rows_want.keys();
        let mut keys_got = rows_got.keys();
        loop {
          let ok = match (keys_want.next(), keys_got.next()) {
            (Some(want), Some(got)) => want == got,
            (Some(_), None) | (None, Some(_)) => false,
            (None, None) => break,
          };
          if !ok {
            return Err(loc.wrap(Error::TyMismatch(
              Ty::Record(rows_want),
              Ty::Record(rows_got),
            )));
          }
        }
        for (lab, want) in rows_want {
          let got = rows_got.remove(&lab).unwrap();
          self.unify(loc, want, got)?;
        }
        Ok(())
      }
      (Ty::Arrow(arg_want, res_want), Ty::Arrow(arg_got, res_got)) => {
        self.unify(loc, *arg_want, *arg_got)?;
        self.unify(loc, *res_want, *res_got)?;
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
          self.unify(loc, want, got)?;
        }
        Ok(())
      }
      (want @ Ty::Record(..), got) | (want @ Ty::Arrow(..), got) | (want @ Ty::Ctor(..), got) => {
        Err(loc.wrap(Error::TyMismatch(want, got)))
      }
    }
  }
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
  pub const fn base(name: StrRef) -> Self {
    Self::Ctor(Vec::new(), Sym::base(name))
  }

  pub fn list(elem: Self) -> Self {
    Self::Ctor(vec![elem], Sym::base(StrRef::LIST))
  }

  pub fn ref_(elem: Self) -> Self {
    Self::Ctor(vec![elem], Sym::base(StrRef::REF))
  }

  pub fn pair(lhs: Self, rhs: Self) -> Self {
    Self::Record(btreemap![Label::Num(1) => lhs, Label::Num(2) => rhs])
  }

  pub fn ty_names(&self) -> TyNameSet {
    match self {
      Self::Var(_) => TyNameSet::new(),
      Self::Record(rows) => rows.iter().flat_map(|(_, ty)| ty.ty_names()).collect(),
      Self::Arrow(arg, res) => arg.ty_names().into_iter().chain(res.ty_names()).collect(),
      Self::Ctor(args, sym) => std::iter::once(sym.name)
        .chain(args.iter().flat_map(Self::ty_names))
        .collect(),
    }
  }

  pub fn apply(&mut self, subst: &Subst) {
    match self {
      Self::Var(tv) => match subst.inner.get(tv) {
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

  pub fn free_ty_vars(&self) -> TyVarSet {
    match self {
      Self::Var(tv) => hashset![*tv],
      Self::Record(rows) => rows.iter().flat_map(|(_, ty)| ty.free_ty_vars()).collect(),
      Self::Arrow(lhs, rhs) => lhs
        .free_ty_vars()
        .union(&rhs.free_ty_vars())
        .copied()
        .collect(),
      Self::Ctor(args, _) => args.iter().flat_map(Self::free_ty_vars).collect(),
    }
  }

  pub const CHAR: Self = Self::base(StrRef::CHAR);
  pub const EXN: Self = Self::base(StrRef::EXN);
  pub const BOOL: Self = Self::base(StrRef::BOOL);
  pub const STRING: Self = Self::base(StrRef::STRING);
  pub const WORD: Self = Self::base(StrRef::WORD);
  pub const INT: Self = Self::base(StrRef::INT);
  pub const REAL: Self = Self::base(StrRef::REAL);
  pub const ORDER: Self = Self::base(StrRef::ORDER);
}

/// A type scheme, a 'forall' type.
#[derive(Clone)]
pub struct TyScheme {
  pub ty_vars: Vec<TyVar>,
  pub ty: Ty,
  /// See `instantiate` and `statics::get`.
  pub overload: Option<Vec<StrRef>>,
}

impl TyScheme {
  pub fn mono(ty: Ty) -> Self {
    Self {
      ty_vars: Vec::new(),
      ty,
      overload: None,
    }
  }

  pub fn apply(&mut self, subst: &Subst) {
    let mut subst = subst.clone();
    for tv in self.ty_vars.iter() {
      subst.inner.remove(tv);
    }
    self.ty.apply(&subst)
  }

  pub fn free_ty_vars(&self) -> TyVarSet {
    self
      .ty
      .free_ty_vars()
      .difference(&self.ty_vars.iter().copied().collect())
      .copied()
      .collect()
  }

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

/// Information about a type that 'has been generated', like a datatype or a a `type t` in a
/// signature.
pub struct SymTyInfo {
  pub ty_fcn: TyFcn,
  pub val_env: ValEnv,
}

pub type SymTys = HashMap<Sym, SymTyInfo>;

#[derive(Clone)]
pub enum TyInfo {
  Alias(TyFcn),
  Sym(Sym),
}

impl TyInfo {
  pub fn ty_fcn<'a>(&'a self, sym_tys: &'a SymTys) -> &'a TyFcn {
    match self {
      TyInfo::Alias(ty_fcn) => ty_fcn,
      TyInfo::Sym(sym) => &sym_tys.get(sym).unwrap().ty_fcn,
    }
  }
}

pub type StrEnv = BTreeMap<StrRef, Env>;

#[derive(Clone, Default)]
pub struct TyEnv {
  pub inner: BTreeMap<StrRef, TyInfo>,
}

impl TyEnv {
  pub fn apply(&mut self, subst: &Subst, sym_tys: &mut SymTys) {
    for (_, ty_info) in self.inner.iter_mut() {
      match ty_info {
        TyInfo::Alias(ty_fcn) => ty_fcn.apply(subst),
        TyInfo::Sym(sym) => sym_tys.get_mut(sym).unwrap().ty_fcn.apply(subst),
      }
    }
  }

  pub fn free_ty_vars(&self, sym_tys: &SymTys) -> TyVarSet {
    self
      .inner
      .iter()
      .flat_map(|(_, ty_info)| ty_info.ty_fcn(sym_tys).free_ty_vars())
      .collect()
  }
}

#[derive(Clone, Copy)]
pub enum IdStatus {
  Ctor,
  Exn,
  Val,
}

impl IdStatus {
  pub fn is_val(&self) -> bool {
    matches!(self, Self::Val)
  }

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

#[derive(Clone)]
pub struct ValInfo {
  pub ty_scheme: TyScheme,
  pub id_status: IdStatus,
}

impl ValInfo {
  pub fn ctor(ty_scheme: TyScheme) -> Self {
    Self {
      ty_scheme,
      id_status: IdStatus::Ctor,
    }
  }

  pub fn exn() -> Self {
    Self {
      ty_scheme: TyScheme::mono(Ty::EXN),
      id_status: IdStatus::Exn,
    }
  }

  pub fn exn_fn(ty: Ty) -> Self {
    Self {
      ty_scheme: TyScheme::mono(Ty::Arrow(ty.into(), Ty::EXN.into())),
      id_status: IdStatus::Exn,
    }
  }

  pub fn val(ty_scheme: TyScheme) -> Self {
    Self {
      ty_scheme,
      id_status: IdStatus::Val,
    }
  }
}

pub type ValEnv = BTreeMap<StrRef, ValInfo>;

#[derive(Clone, Default)]
pub struct Env {
  pub str_env: StrEnv,
  pub ty_env: TyEnv,
  pub val_env: ValEnv,
}

impl Env {
  /// `other` overwrites `self`.
  pub fn extend(&mut self, other: Self) {
    for (name, env) in other.str_env {
      self.str_env.insert(name, env);
    }
    for (name, ty_info) in other.ty_env.inner {
      self.ty_env.inner.insert(name, ty_info);
    }
    for (name, val_info) in other.val_env {
      self.val_env.insert(name, val_info);
    }
  }

  pub fn maybe_extend(&mut self, other: Self, loc: Loc) -> Result<()> {
    for (name, env) in other.str_env {
      if self.str_env.insert(name, env).is_some() {
        return Err(loc.wrap(Error::Redefined(name)));
      }
    }
    for (name, ty_info) in other.ty_env.inner {
      if self.ty_env.inner.insert(name, ty_info).is_some() {
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

  pub fn ty_names(&self) -> TyNameSet {
    self
      .str_env
      .iter()
      .flat_map(|(_, env)| env.ty_names())
      .chain(self.ty_env.inner.keys().copied())
      .collect()
  }

  pub fn apply(&mut self, subst: &Subst, sym_tys: &mut SymTys) {
    for (_, env) in self.str_env.iter_mut() {
      env.apply(subst, sym_tys);
    }
    self.ty_env.apply(subst, sym_tys);
    for (_, val_info) in self.val_env.iter_mut() {
      val_info.ty_scheme.apply(subst);
    }
  }

  pub fn free_ty_vars(&self, sym_tys: &SymTys) -> TyVarSet {
    self
      .str_env
      .iter()
      .flat_map(|(_, env)| env.free_ty_vars(sym_tys))
      .chain(self.ty_env.free_ty_vars(sym_tys))
      .chain(
        self
          .val_env
          .iter()
          .flat_map(|(_, vi)| vi.ty_scheme.free_ty_vars()),
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

pub type TyNameSet = HashSet<StrRef>;

pub type TyVarSet = HashSet<TyVar>;

#[derive(Clone)]
pub struct Cx {
  pub ty_names: TyNameSet,
  pub ty_vars: TyVarSet,
  pub env: Env,
}

impl Cx {
  // this is the o-plus operation defined in the Definition, which extends a context by an
  // environment and that environment's type name set.
  pub fn o_plus(&mut self, env: Env) {
    let ty_names = env.ty_names();
    self.env.extend(env);
    self.ty_names.extend(ty_names);
  }
}

#[derive(Clone)]
pub struct Sig {
  pub ty_names: TyNameSet,
  pub env: Env,
}

#[derive(Clone)]
pub struct FunSig {
  pub ty_names: TyNameSet,
  pub env: Env,
  pub sig: Sig,
}

pub type SigEnv = HashMap<StrRef, Sig>;

pub type FunEnv = HashMap<StrRef, FunSig>;

#[derive(Clone)]
pub struct Basis {
  pub ty_names: TyNameSet,
  pub fun_env: FunEnv,
  pub sig_env: SigEnv,
  pub env: Env,
}

impl Basis {
  pub fn apply(&mut self, subst: &Subst, sym_tys: &mut SymTys) {
    for (_, fun_sig) in self.fun_env.iter_mut() {
      fun_sig.env.apply(subst, sym_tys);
      fun_sig.sig.env.apply(subst, sym_tys);
    }
    for (_, sig) in self.sig_env.iter_mut() {
      sig.env.apply(subst, sym_tys);
    }
    self.env.apply(subst, sym_tys);
  }

  pub fn free_ty_vars(&self, sym_tys: &SymTys) -> TyVarSet {
    self
      .fun_env
      .iter()
      .flat_map(|(_, fun_sig)| {
        fun_sig
          .env
          .free_ty_vars(sym_tys)
          .into_iter()
          .chain(fun_sig.sig.env.free_ty_vars(sym_tys))
      })
      .chain(
        self
          .sig_env
          .iter()
          .flat_map(|(_, sig)| sig.env.free_ty_vars(sym_tys)),
      )
      .chain(self.env.free_ty_vars(sym_tys))
      .collect()
  }

  pub fn to_cx(&self) -> Cx {
    Cx {
      ty_names: self.ty_names.clone(),
      ty_vars: TyVarSet::new(),
      env: self.env.clone(),
    }
  }

  pub fn add_env(&mut self, env: Env) {
    let ty_names = env.ty_names();
    self.ty_names.extend(ty_names);
    self.env.extend(env);
  }

  pub fn add_sig_env(&mut self, sig_env: SigEnv) {
    let ty_names = sig_env
      .iter()
      .flat_map(|(_, sig)| sig.ty_names.iter())
      .copied();
    self.ty_names.extend(ty_names);
    self.sig_env.extend(sig_env);
  }

  pub fn add_fun_env(&mut self, fun_env: FunEnv) {
    let ty_names = fun_env
      .iter()
      .flat_map(|(_, sig)| sig.ty_names.iter())
      .copied();
    self.ty_names.extend(ty_names);
    self.fun_env.extend(fun_env);
  }
}

/// The state passed around by many of the statics functions. There's only one of these, and it's
/// constantly being mutably, additively updated as we go.
#[derive(Default)]
pub struct State {
  /// The next type variable ID to hand out.
  next_ty_var: usize,
  /// The next symbol ID to hand out.
  next_sym: usize,
  /// The overload constraints. These constraints are solved at the very end.
  pub overload: Vec<(Loc, TyVar, Vec<StrRef>)>,
  /// The substitution, the unifier of the entire program.
  pub subst: Subst,
  /// The types that 'have been generated' and information about them.
  pub sym_tys: SymTys,
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
}

/// A pattern, for the purposes of static analysis. See exhaustive.rs.
#[derive(Debug, Clone)]
pub enum Pat {
  Anything,
  Con(Con, Vec<Pat>),
}

impl Pat {
  pub fn zero(con: Con) -> Self {
    Self::Con(con, vec![])
  }

  /// Requires the patterns be sorted by label.
  pub fn record(mut pats: Vec<Pat>) -> Self {
    if pats.len() == 1 {
      // may happen in the desugaring of `Fun`.
      pats.pop().unwrap()
    } else {
      Self::Con(Con::Record(pats.len()), pats)
    }
  }
}

/// Bit confusing to have both 'Con' and 'Ctor'. We originally used 'Ctor' to mean 'constructor',
/// and then adopted 'Con' as well from reading the paper which was the basis of the exhaustiveness
/// checker.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Con {
  Int(i32),
  Word(i32),
  String(StrRef),
  Char(u8),
  /// This should never be used directly, use `Pat::record` instead. The usize is the arity.
  Record(usize),
  /// A constructor from a datatype or an exception.
  Ctor(StrRef, Span),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Span {
  Finite(usize),
  PosInf,
}

impl Con {
  pub fn span(&self) -> Span {
    match *self {
      Self::Int(_) | Self::Word(_) | Self::String(_) => Span::PosInf,
      Self::Char(_) => Span::Finite(256),
      Self::Record(_) => Span::Finite(1),
      Self::Ctor(_, s) => s,
    }
  }
}

#[test]
fn char_span() {
  // The span of a Con::Char is 256 = 2^8 since a char is a u8. This test and the definition of
  // `Con#span` should change if Char ever becomes not a u8.
  assert_eq!(Con::Char(0u8).span(), Span::Finite(256));
}
