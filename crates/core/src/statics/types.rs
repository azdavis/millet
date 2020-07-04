//! Types used in static analysis, many essentially as describe in the Definition.

use crate::ast::Label;
use crate::intern::{StrRef, StrStore};
use crate::loc::{Loc, Located};
use maplit::{hashmap, hashset};
use std::collections::{HashMap, HashSet};
use std::fmt;

pub enum StaticsError {
  Undefined(Item, StrRef),
  Redefined(StrRef),
  DuplicateLabel(Label),
  Circularity(TyVar, Ty),
  HeadMismatch(Ty, Ty),
  MissingLabel(Label),
  ValAsPat,
  WrongNumTyArgs(usize, usize),
  NonVarInAs(StrRef),
  ForbiddenBinding(StrRef),
  NoSuitableOverload,
  TyNameEscape,
  NonExhaustiveMatch,
  NonExhaustiveBinding,
  UnreachablePattern,
  Todo,
}

impl StaticsError {
  pub fn message(&self, store: &StrStore) -> String {
    match self {
      Self::Undefined(item, id) => format!("undefined {} identifier: {}", item, store.get(*id)),
      Self::Redefined(id) => format!("redefined identifier: {}", store.get(*id)),
      Self::DuplicateLabel(lab) => format!("duplicate label: {}", show_lab(store, *lab)),
      Self::Circularity(ty_var, ty) => {
        format!("circularity: {:?} in {}", ty_var, show_ty(store, &ty))
      }
      Self::HeadMismatch(lhs, rhs) => format!(
        "mismatched types: expected {}, found {}",
        show_ty(store, &lhs),
        show_ty(store, &rhs)
      ),
      Self::MissingLabel(lab) => format!("type is missing label {}", show_lab(store, *lab)),
      Self::ValAsPat => "value binding used as pattern".to_owned(),
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
      Self::Todo => "unimplemented language construct".to_owned(),
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

pub type Result<T> = std::result::Result<T, Located<StaticsError>>;

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

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct TyVar {
  id: usize,
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

#[derive(Clone, Default)]
pub struct Subst {
  inner: HashMap<TyVar, Ty>,
}

impl Subst {
  pub fn insert(&mut self, tv: TyVar, ty: Ty) {
    for (_, other) in self.inner.iter_mut() {
      other.apply(&Self {
        inner: hashmap![tv => ty.clone()],
      });
    }
    assert!(self.inner.insert(tv, ty).is_none());
  }

  fn bind(&mut self, loc: Loc, tv: TyVar, ty: Ty) -> Result<()> {
    if let Ty::Var(other) = ty {
      if tv == other {
        return Ok(());
      }
    }
    if ty.free_ty_vars().contains(&tv) {
      return Err(loc.wrap(StaticsError::Circularity(tv, ty)));
    }
    self.insert(tv, ty);
    Ok(())
  }

  /// lhs = expected, rhs = found. the types immediately have self applied to them upon entry to
  /// this function, so no need to do it yourself before calling.
  pub fn unify(&mut self, loc: Loc, mut lhs: Ty, mut rhs: Ty) -> Result<()> {
    lhs.apply(self);
    rhs.apply(self);
    match (lhs, rhs) {
      (Ty::Var(tv), rhs) => self.bind(loc, tv, rhs),
      (lhs, Ty::Var(tv)) => self.bind(loc, tv, lhs),
      (Ty::Record(rows_l), Ty::Record(rows_r)) => {
        let mut map_l: HashMap<_, _> = rows_l.into_iter().collect();
        let mut map_r: HashMap<_, _> = rows_r.into_iter().collect();
        let keys: HashSet<_> = map_l.keys().chain(map_r.keys()).copied().collect();
        for k in keys {
          match (map_l.remove(&k), map_r.remove(&k)) {
            (Some(ty_l), Some(ty_r)) => self.unify(loc, ty_l, ty_r)?,
            (Some(..), None) | (None, Some(..)) => {
              return Err(loc.wrap(StaticsError::MissingLabel(k)))
            }
            (None, None) => unreachable!(),
          }
        }
        Ok(())
      }
      (Ty::Arrow(arg_l, res_l), Ty::Arrow(arg_r, res_r)) => {
        self.unify(loc, *arg_l, *arg_r)?;
        self.unify(loc, *res_l, *res_r)?;
        Ok(())
      }
      (Ty::Ctor(args_l, name_l), Ty::Ctor(args_r, name_r)) => {
        if name_l != name_r {
          return Err(loc.wrap(StaticsError::HeadMismatch(
            Ty::Ctor(args_l, name_l),
            Ty::Ctor(args_r, name_r),
          )));
        }
        assert_eq!(args_l.len(), args_r.len(), "mismatched Ctor args len");
        for (arg_l, arg_r) in args_l.into_iter().zip(args_r) {
          self.unify(loc, arg_l, arg_r)?;
        }
        Ok(())
      }
      (lhs @ Ty::Record(..), rhs) | (lhs @ Ty::Arrow(..), rhs) | (lhs @ Ty::Ctor(..), rhs) => {
        Err(loc.wrap(StaticsError::HeadMismatch(lhs, rhs)))
      }
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
  /// TyVar
  Var(TyVar),
  /// RowType. Use a Vec to preserve source order. TODO row polymorphism?
  Record(Vec<(Label, Ty)>),
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
        for (_, ty) in rows {
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

pub type TyFcn = TyScheme;

pub struct DatatypeInfo {
  pub ty_fcn: TyFcn,
  pub val_env: ValEnv,
}

pub type Datatypes = HashMap<Sym, DatatypeInfo>;

#[derive(Clone)]
pub enum TyInfo {
  Alias(TyFcn),
  Datatype(Sym),
}

impl TyInfo {
  pub fn ty_fcn<'a>(&'a self, dts: &'a Datatypes) -> &'a TyFcn {
    match self {
      TyInfo::Alias(ty_fcn) => ty_fcn,
      TyInfo::Datatype(sym) => &dts.get(sym).unwrap().ty_fcn,
    }
  }
}

pub type StrEnv = HashMap<StrRef, Env>;

#[derive(Clone, Default)]
pub struct TyEnv {
  pub inner: HashMap<StrRef, TyInfo>,
}

impl TyEnv {
  pub fn apply(&mut self, subst: &Subst, dts: &mut Datatypes) {
    for (_, ty_info) in self.inner.iter_mut() {
      match ty_info {
        TyInfo::Alias(ty_fcn) => ty_fcn.apply(subst),
        TyInfo::Datatype(sym) => dts.get_mut(sym).unwrap().ty_fcn.apply(subst),
      }
    }
  }

  pub fn free_ty_vars(&self, dts: &Datatypes) -> TyVarSet {
    self
      .inner
      .iter()
      .flat_map(|(_, ty_info)| ty_info.ty_fcn(dts).free_ty_vars())
      .collect()
  }
}

#[derive(Clone)]
pub enum IdStatus {
  Ctor,
  Exn,
  Val,
}

impl IdStatus {
  pub fn is_val(&self) -> bool {
    matches!(self, Self::Val)
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

  pub fn val(ty_scheme: TyScheme) -> Self {
    Self {
      ty_scheme,
      id_status: IdStatus::Val,
    }
  }
}

pub type ValEnv = HashMap<StrRef, ValInfo>;

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

  pub fn ty_names(&self) -> TyNameSet {
    self
      .str_env
      .iter()
      .flat_map(|(_, env)| env.ty_names())
      .chain(self.ty_env.inner.keys().copied())
      .collect()
  }

  pub fn apply(&mut self, subst: &Subst, dts: &mut Datatypes) {
    for (_, env) in self.str_env.iter_mut() {
      env.apply(subst, dts);
    }
    self.ty_env.apply(subst, dts);
    for (_, val_info) in self.val_env.iter_mut() {
      val_info.ty_scheme.apply(subst);
    }
  }

  pub fn free_ty_vars(&self, dts: &Datatypes) -> TyVarSet {
    self
      .str_env
      .iter()
      .flat_map(|(_, env)| env.free_ty_vars(dts))
      .chain(self.ty_env.free_ty_vars(dts))
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

pub struct Sig {
  pub ty_names: TyNameSet,
  pub env: Env,
}

pub struct FunSig {
  pub ty_names: TyNameSet,
  pub env: Env,
  pub sig: Sig,
}

pub type SigEnv = HashMap<StrRef, Sig>;

pub type FunEnv = HashMap<StrRef, FunSig>;

pub struct Basis {
  pub ty_names: TyNameSet,
  pub fun_env: FunEnv,
  pub sig_env: SigEnv,
  pub env: Env,
}

impl Basis {
  pub fn apply(&mut self, subst: &Subst, dts: &mut Datatypes) {
    for (_, fun_sig) in self.fun_env.iter_mut() {
      fun_sig.env.apply(subst, dts);
      fun_sig.sig.env.apply(subst, dts);
    }
    for (_, sig) in self.sig_env.iter_mut() {
      sig.env.apply(subst, dts);
    }
    self.env.apply(subst, dts);
  }

  pub fn free_ty_vars(&self, dts: &Datatypes) -> TyVarSet {
    self
      .fun_env
      .iter()
      .flat_map(|(_, fun_sig)| {
        fun_sig
          .env
          .free_ty_vars(dts)
          .into_iter()
          .chain(fun_sig.sig.env.free_ty_vars(dts))
      })
      .chain(
        self
          .sig_env
          .iter()
          .flat_map(|(_, sig)| sig.env.free_ty_vars(dts)),
      )
      .chain(self.env.free_ty_vars(dts))
      .collect()
  }
}

#[derive(Default)]
pub struct State {
  next_ty_var: usize,
  next_sym: usize,
  pub overload: Vec<(Loc, TyVar, Vec<StrRef>)>,
  pub subst: Subst,
  pub datatypes: Datatypes,
}

impl State {
  pub fn new_ty_var(&mut self, equality: bool) -> TyVar {
    let id = self.next_ty_var;
    self.next_ty_var += 1;
    TyVar { id, equality }
  }

  pub fn new_sym(&mut self, name: Located<StrRef>) -> Sym {
    let id = Some(name.loc.wrap(self.next_sym));
    self.next_sym += 1;
    Sym { id, name: name.val }
  }
}

#[derive(Debug, Clone)]
pub enum Pat {
  Anything,
  Int(i32),
  Word(i32),
  String(StrRef),
  Char(u8),
  Record(Vec<(Label, Pat)>),
  Ctor(StrRef, Option<Box<Pat>>),
}
