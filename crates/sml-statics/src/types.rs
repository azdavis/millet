//! Types.
//!
//! Probably the single most important file in this crate. Lots of types used pervasively across
//! this crate are defined here.

use crate::fmt_util::{idx_to_name, ty_var_name};
use drop_bomb::DropBomb;
use fast_hash::{FxHashMap, FxHashSet};
use fmt_util::comma_seq;
use std::collections::BTreeMap;
use std::fmt;
use std::sync::Arc;
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

  pub(crate) fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
  ) -> impl fmt::Display + 'a {
    TyDisplay { ty: self, bound_vars: None, meta_vars, syms, prec: TyPrec::Arrow }
  }
}

struct TyDisplay<'a> {
  ty: &'a Ty,
  bound_vars: Option<&'a BoundTyVars>,
  meta_vars: &'a MetaVarNames<'a>,
  syms: &'a Syms,
  prec: TyPrec,
}

impl<'a> TyDisplay<'a> {
  fn with(&self, ty: &'a Ty, prec: TyPrec) -> Self {
    Self { ty, bound_vars: self.bound_vars, meta_vars: self.meta_vars, syms: self.syms, prec }
  }
}

impl<'a> fmt::Display for TyDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty {
      Ty::None => f.write_str("_")?,
      Ty::BoundVar(bv) => {
        let vars = self.bound_vars.expect("bound ty var without a BoundTyVars");
        let equality = matches!(vars.0[bv.0], Some(TyVarKind::Equality));
        let name = ty_var_name(equality, bv.0);
        write!(f, "{name}")?;
      }
      Ty::MetaVar(mv) => {
        let name = self.meta_vars.get(*mv).ok_or(fmt::Error)?;
        write!(f, "{name}")?;
      }
      Ty::FixedVar(fv) => fv.fmt(f)?,
      Ty::Record(rows) => {
        if rows.is_empty() {
          return f.write_str("unit");
        }
        let is_tuple = rows.len() > 1
          && rows.keys().enumerate().all(|(idx, lab)| sml_hir::Lab::tuple(idx) == *lab);
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
          comma_seq(
            f,
            rows.iter().map(|(lab, ty)| RowDisplay {
              bound_vars: self.bound_vars,
              meta_vars: self.meta_vars,
              syms: self.syms,
              lab,
              ty,
            }),
          )?;
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
        match self.syms.get(*sym) {
          None => f.write_str("exn")?,
          Some((name, _)) => name.fmt(f)?,
        }
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

#[derive(Debug)]
pub(crate) struct MetaVarNames<'a> {
  next_idx: usize,
  map: FxHashMap<MetaTyVar, MetaVarName>,
  info: &'a MetaVarInfo,
}

impl<'a> MetaVarNames<'a> {
  pub(crate) fn new(info: &'a MetaVarInfo) -> Self {
    Self { next_idx: 0, map: FxHashMap::default(), info }
  }

  pub(crate) fn extend_for(&mut self, ty: &Ty) {
    meta_vars(
      &Subst::default(),
      &mut |x, _| {
        self.map.entry(x).or_insert_with(|| {
          let ret = MetaVarName::Idx(self.next_idx);
          self.next_idx += 1;
          ret
        });
      },
      ty,
    );
  }

  /// tries the [`MetaVarInfo`] first, then fall back to a generated name like `?a`, `?b`, etc.
  pub(crate) fn get(&self, mv: MetaTyVar) -> Option<MetaVarName> {
    self
      .info
      .0
      .get(&mv)
      .and_then(|kind| match kind {
        TyVarKind::Overloaded(ov) => Some(MetaVarName::Overload(*ov)),
        TyVarKind::Equality | TyVarKind::Record(_) => None,
      })
      .or_else(|| self.map.get(&mv).copied())
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum MetaVarName {
  Idx(usize),
  Overload(Overload),
}

impl fmt::Display for MetaVarName {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match *self {
      MetaVarName::Idx(idx) => {
        f.write_str("?")?;
        for c in idx_to_name(idx) {
          write!(f, "{c}")?;
        }
        Ok(())
      }
      MetaVarName::Overload(ov) => ov.fmt(f),
    }
  }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  Arrow,
  Star,
  App,
}

struct RowDisplay<'a> {
  bound_vars: Option<&'a BoundTyVars>,
  meta_vars: &'a MetaVarNames<'a>,
  syms: &'a Syms,
  lab: &'a sml_hir::Lab,
  ty: &'a Ty,
}

impl<'a> fmt::Display for RowDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(self.lab, f)?;
    f.write_str(" : ")?;
    let td = TyDisplay {
      ty: self.ty,
      bound_vars: self.bound_vars,
      meta_vars: self.meta_vars,
      syms: self.syms,
      prec: TyPrec::Arrow,
    };
    fmt::Display::fmt(&td, f)
  }
}

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
    let (ty, kind) = f(Ty::BoundVar(BoundTyVar(0)));
    Self { bound_vars: BoundTyVars(vec![kind]), ty }
  }

  pub(crate) fn n_ary<I>(iter: I, sym: Sym) -> Self
  where
    I: Iterator<Item = Option<TyVarKind>>,
  {
    let bound_vars = BoundTyVars(iter.collect());
    let ty = Ty::Con((0..bound_vars.len()).map(|i| Ty::BoundVar(BoundTyVar(i))).collect(), sym);
    Self { bound_vars, ty }
  }

  pub(crate) fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      ty: &self.ty,
      bound_vars: Some(&self.bound_vars),
      meta_vars,
      syms,
      prec: TyPrec::Arrow,
    }
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct BoundTyVars(Vec<Option<TyVarKind>>);

impl BoundTyVars {
  pub(crate) fn len(&self) -> usize {
    self.0.len()
  }

  pub(crate) fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub(crate) fn kinds(&self) -> impl Iterator<Item = &Option<TyVarKind>> + '_ {
    self.0.iter()
  }
}

#[derive(Debug, Clone)]
pub(crate) enum TyVarKind {
  Equality,
  Overloaded(Overload),
  Record(RecordTy),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BasicOverload {
  Int,
  Real,
  Word,
  String,
  Char,
}

impl BasicOverload {
  pub(crate) fn as_str(self) -> &'static str {
    match self {
      BasicOverload::Int => "int",
      BasicOverload::Real => "real",
      BasicOverload::Word => "word",
      BasicOverload::String => "string",
      BasicOverload::Char => "char",
    }
  }
}

impl fmt::Display for BasicOverload {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.as_str())
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum CompositeOverload {
  WordInt,
  RealInt,
  Num,
  NumTxt,
}

impl CompositeOverload {
  pub(crate) fn as_basics(self) -> &'static [BasicOverload] {
    match self {
      Self::WordInt => &[BasicOverload::Word, BasicOverload::Int],
      Self::RealInt => &[BasicOverload::Real, BasicOverload::Int],
      Self::Num => &[BasicOverload::Word, BasicOverload::Real, BasicOverload::Int],
      Self::NumTxt => &[
        BasicOverload::Word,
        BasicOverload::Real,
        BasicOverload::Int,
        BasicOverload::String,
        BasicOverload::Char,
      ],
    }
  }

  pub(crate) fn unify(self, other: Self) -> Overload {
    match (self, other) {
      (Self::WordInt, Self::WordInt | Self::Num | Self::NumTxt)
      | (Self::Num | Self::NumTxt, Self::WordInt) => Overload::Composite(Self::WordInt),
      (Self::WordInt, Self::RealInt) | (Self::RealInt, Self::WordInt) => {
        Overload::Basic(BasicOverload::Int)
      }
      (Self::RealInt, Self::RealInt | Self::Num | Self::NumTxt)
      | (Self::Num | Self::NumTxt, Self::RealInt) => Overload::Composite(Self::RealInt),
      (Self::Num, Self::Num | Self::NumTxt) | (Self::NumTxt, Self::Num) => {
        Overload::Composite(Self::Num)
      }
      (Self::NumTxt, Self::NumTxt) => Overload::Composite(Self::NumTxt),
    }
  }
}

impl fmt::Display for CompositeOverload {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      CompositeOverload::WordInt => f.write_str("<wordint>"),
      CompositeOverload::RealInt => f.write_str("<realint>"),
      CompositeOverload::Num => f.write_str("<num>"),
      CompositeOverload::NumTxt => f.write_str("<numtxt>"),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Overload {
  Basic(BasicOverload),
  Composite(CompositeOverload),
}

impl Overload {
  pub(crate) fn as_basics(&self) -> &[BasicOverload] {
    match self {
      Overload::Basic(b) => std::slice::from_ref(b),
      Overload::Composite(c) => c.as_basics(),
    }
  }

  /// returns `None` iff the overloads could not be unified.
  pub(crate) fn unify(self, other: Self) -> Option<Self> {
    match (self, other) {
      (Self::Basic(b1), Self::Basic(b2)) => {
        if b1 == b2 {
          Some(Self::Basic(b1))
        } else {
          None
        }
      }
      (Self::Basic(b), Self::Composite(c)) | (Self::Composite(c), Self::Basic(b)) => {
        c.as_basics().iter().find(|&&x| x == b).copied().map(Self::Basic)
      }
      (Self::Composite(c1), Self::Composite(c2)) => Some(c1.unify(c2)),
    }
  }
}

impl fmt::Display for Overload {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Overload::Basic(b) => b.fmt(f),
      Overload::Composite(c) => c.fmt(f),
    }
  }
}

/// Information about overloads.
#[derive(Debug, Default, Clone)]
pub(crate) struct Overloads {
  pub(crate) int: Vec<Sym>,
  pub(crate) real: Vec<Sym>,
  pub(crate) word: Vec<Sym>,
  pub(crate) string: Vec<Sym>,
  pub(crate) char: Vec<Sym>,
}

impl std::ops::Index<BasicOverload> for Overloads {
  type Output = Vec<Sym>;

  fn index(&self, index: BasicOverload) -> &Self::Output {
    match index {
      BasicOverload::Int => &self.int,
      BasicOverload::Real => &self.real,
      BasicOverload::Word => &self.word,
      BasicOverload::String => &self.string,
      BasicOverload::Char => &self.char,
    }
  }
}

impl std::ops::IndexMut<BasicOverload> for Overloads {
  fn index_mut(&mut self, index: BasicOverload) -> &mut Self::Output {
    match index {
      BasicOverload::Int => &mut self.int,
      BasicOverload::Real => &mut self.real,
      BasicOverload::Word => &mut self.word,
      BasicOverload::String => &mut self.string,
      BasicOverload::Char => &mut self.char,
    }
  }
}

/// Definition: `TyVar`
///
/// But only kind of. There's also:
///
/// - [`MetaTyVar`]
/// - [`FixedTyVar`]
/// - [`sml_hir::TyVar`]
///
/// Basically a de Bruijn index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct BoundTyVar(usize);

impl BoundTyVar {
  pub(crate) fn index_into<'a, T>(&self, xs: &'a [T]) -> &'a T {
    xs.get(self.0).unwrap()
  }
}

/// Generated, and to be solved for a real type, by the inference algorithm.
///
/// Should eventually be solved in a [`Subst`], but before that, it may be "restricted" by the
/// `Subst` without yet being fully solved to a type.
///
/// Internally contains a "rank" to know when it should be generalizable; see "Efficient ML Type
/// Inference Using Ranked Type Variables" (doi:10.1145/1292535.1292538)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct MetaTyVar {
  id: u32,
  rank: MetaTyVarRank,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum MetaTyVarRank {
  Finite(u16),
  Infinite,
}

impl MetaTyVar {
  pub(crate) fn rank(self) -> MetaTyVarRank {
    self.rank
  }
}

#[derive(Debug, Default)]
pub(crate) struct MetaTyVarGen {
  id: u32,
  rank: u16,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Generalizable {
  Sometimes,
  Always,
}

impl MetaTyVarGen {
  pub(crate) fn gen(&mut self, g: Generalizable) -> MetaTyVar {
    let ret = MetaTyVar {
      id: self.id,
      rank: match g {
        Generalizable::Sometimes => MetaTyVarRank::Finite(self.rank),
        Generalizable::Always => MetaTyVarRank::Infinite,
      },
    };
    self.id += 1;
    ret
  }

  pub(crate) fn inc_rank(&mut self) {
    self.rank += 1;
  }

  pub(crate) fn dec_rank(&mut self) {
    self.rank -= 1;
  }

  pub(crate) fn generalizer(&self) -> MetaTyVarGeneralizer {
    MetaTyVarGeneralizer { rank: self.rank }
  }

  pub(crate) fn gen_same_rank(&mut self, mv: MetaTyVar) -> MetaTyVar {
    let ret = MetaTyVar { id: self.id, rank: mv.rank };
    self.id += 1;
    ret
  }
}

#[derive(Debug)]
pub(crate) struct MetaTyVarGeneralizer {
  rank: u16,
}

impl MetaTyVarGeneralizer {
  /// Returns in O(1) time.
  fn is_generalizable(&self, mv: MetaTyVar) -> bool {
    match mv.rank {
      MetaTyVarRank::Finite(r) => r > self.rank,
      MetaTyVarRank::Infinite => true,
    }
  }
}

/// Corresponds to a user written type variable.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct FixedTyVar {
  id: Uniq,
  ty_var: sml_hir::TyVar,
  src: TyVarSrc,
}

impl FixedTyVar {
  pub(crate) fn ty_var(&self) -> &sml_hir::TyVar {
    &self.ty_var
  }

  pub(crate) fn src(&self) -> TyVarSrc {
    self.src
  }
}

impl fmt::Display for FixedTyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.ty_var.fmt(f)
  }
}

/// Where a type variable was bound.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum TyVarSrc {
  /// Bound at `type` or `datatype` (or `where type`).
  Ty,
  /// Bound at `val` (or `fun`).
  Val,
}

#[derive(Debug, Default)]
pub(crate) struct FixedTyVarGen(UniqGen);

impl FixedTyVarGen {
  pub(crate) fn gen(&mut self, ty_var: sml_hir::TyVar, src: TyVarSrc) -> FixedTyVar {
    FixedTyVar { id: self.0.gen(), ty_var, src }
  }
}

pub(crate) type RecordTy = BTreeMap<sml_hir::Lab, Ty>;

/// Definition: `TyName`
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Sym(usize);

impl fmt::Debug for Sym {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut dt = f.debug_tuple("Sym");
    match self.special() {
      None => dt.field(&self.0),
      Some(x) => dt.field(&x),
    };
    dt.finish()
  }
}

macro_rules! mk_special_syms {
  ($( ($idx:expr, $mk_ty:ident, $name:ident, $str:literal), )*) => {
    impl Sym {
      $(
        pub(crate) const $name: Self = Self($idx);
      )*

      pub(crate) fn special(&self) -> Option<&'static str> {
        let s = match *self {
          $(
            Self::$name => $str,
          )*
          _ => return None,
        };
        Some(s)
      }
    }

    impl Ty {
      $(
        mk_special_syms!(@mk_ty, $mk_ty, $name, $idx);
      )*
    }
  };
  (@mk_ty, y, $name:ident, $idx:expr) => {
    pub(crate) const $name: Ty = Ty::zero(Sym::$name);
  };
  (@mk_ty, n, $name:ident, $idx:expr) => {};
}

// keep in sync with `std_basis`. weird symbols (i.e. EXN) must come first. indices must start at 0
// and go up by 1.
mk_special_syms![
  (0, y, EXN, "exn"),
  (1, y, INT, "int"),
  (2, y, WORD, "word"),
  (3, y, REAL, "real"),
  (4, y, CHAR, "char"),
  (5, y, STRING, "string"),
  (6, y, BOOL, "bool"),
  (7, n, LIST, "list"),
  (8, n, REF, "ref"),
];

impl Sym {
  /// there's only 1, and it's EXN.
  const NUM_WEIRD: usize = 1;

  /// never call this on a weird sym.
  fn idx(self) -> usize {
    self.0 - Self::NUM_WEIRD
  }

  /// Returns whether this `Sym` was generated by a [`Syms`] after that `Syms` generated the
  /// `marker`.
  pub(crate) fn generated_after(self, marker: &SymsMarker) -> bool {
    self != Self::EXN && self.idx() >= marker.0
  }
}

/// Information about generated types, generated exceptions, and overload types.
///
/// Note the `Default` impl is "fake", in that it returns a totally empty `Syms`, which will lack
/// even built-in items like `type int` and `exception Bind`.
#[derive(Debug, Default, Clone)]
pub struct Syms {
  /// remember: always use Sym::idx to index
  store: Vec<(sml_hir::Path, TyInfo)>,
  exns: Vec<(sml_hir::Path, Option<Ty>)>,
  overloads: Overloads,
}

impl Syms {
  pub(crate) fn start(&mut self, name: sml_hir::Path) -> StartedSym {
    let ty_info =
      TyInfo { ty_scheme: TyScheme::zero(Ty::None), val_env: ValEnv::default(), def: None };
    self.store.push((name, ty_info));
    StartedSym {
      bomb: DropBomb::new("must be passed to Syms::finish"),
      // calculate len after push, because we sub 1 in get, because of Sym::EXN.
      sym: Sym(self.store.len()),
    }
  }

  pub(crate) fn finish(&mut self, mut started: StartedSym, ty_info: TyInfo) {
    started.bomb.defuse();
    self.store[started.sym.idx()].1 = ty_info;
  }

  /// Returns `None` iff passed `Sym::EXN`.
  pub(crate) fn get(&self, sym: Sym) -> Option<(&sml_hir::Path, &TyInfo)> {
    if sym == Sym::EXN {
      return None;
    }
    let &(ref name, ref info) = self.store.get(sym.idx()).unwrap();
    Some((name, info))
  }

  pub(crate) fn insert_exn(&mut self, name: sml_hir::Path, param: Option<Ty>) -> Exn {
    let ret = Exn(self.exns.len());
    self.exns.push((name, param));
    ret
  }

  pub(crate) fn get_exn(&self, exn: Exn) -> (&sml_hir::Path, Option<&Ty>) {
    let &(ref name, ref param) = self.exns.get(exn.0).unwrap();
    (name, param.as_ref())
  }

  pub(crate) fn mark(&self) -> SymsMarker {
    SymsMarker(self.store.len())
  }

  pub(crate) fn iter(&self) -> impl Iterator<Item = (&sml_hir::Path, &TyInfo)> {
    self.store.iter().map(|&(ref a, ref b)| (a, b))
  }

  pub(crate) fn overloads(&mut self) -> &mut Overloads {
    &mut self.overloads
  }
}

/// A marker to determine when a `Sym` was generated.
pub(crate) struct SymsMarker(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Exn(usize);

/// A helper to construct information about [`Syms`]s.
pub(crate) struct StartedSym {
  bomb: DropBomb,
  sym: Sym,
}

impl StartedSym {
  pub(crate) fn sym(&self) -> Sym {
    self.sym
  }
}

/// Definition: `TyStr`
#[derive(Debug, Clone)]
pub(crate) struct TyInfo {
  pub(crate) ty_scheme: TyScheme,
  pub(crate) val_env: ValEnv,
  pub(crate) def: Option<Def>,
}

/// Definition: `StrEnv`
pub(crate) type StrEnv = FxHashMap<str_util::Name, Env>;

/// Definition: `TyEnv`
pub(crate) type TyEnv = FxHashMap<str_util::Name, TyInfo>;

/// Definition: `ValEnv`
pub(crate) type ValEnv = FxHashMap<str_util::Name, ValInfo>;

#[derive(Debug, Clone)]
pub(crate) struct ValInfo {
  pub(crate) ty_scheme: TyScheme,
  pub(crate) id_status: IdStatus,
  pub(crate) def: Option<Def>,
}

/// Definition: `IdStatus`
#[derive(Debug, Clone, Copy)]
pub(crate) enum IdStatus {
  Con,
  Exn(Exn),
  Val,
}

impl IdStatus {
  pub(crate) fn same_kind_as(&self, other: &Self) -> bool {
    matches!(
      (self, other),
      (Self::Con, Self::Con) | (Self::Exn(_), Self::Exn(_)) | (Self::Val, Self::Val)
    )
  }
}

pub(crate) trait EnvLike {
  fn get_str(&self, name: &str_util::Name) -> Option<&Env>;
  fn get_ty(&self, name: &str_util::Name) -> Option<&TyInfo>;
  fn get_val(&self, name: &str_util::Name) -> Option<&ValInfo>;
  /// empties other into self.
  fn append(&mut self, other: &mut Env);
  fn all_str(&self) -> Vec<&Env>;
  fn all_ty(&self) -> Vec<&TyInfo>;
  fn all_val(&self) -> Vec<&ValInfo>;
  fn into_env(self) -> Env;
}

/// Definition: Env
#[derive(Debug, Default, Clone)]
pub(crate) struct Env {
  pub(crate) str_env: StrEnv,
  pub(crate) ty_env: TyEnv,
  pub(crate) val_env: ValEnv,
  pub(crate) def: Option<Def>,
}

impl Env {
  pub(crate) fn with_def(def: Option<Def>) -> Self {
    Self { def, ..Default::default() }
  }
}

impl EnvLike for Env {
  fn get_str(&self, name: &str_util::Name) -> Option<&Env> {
    self.str_env.get(name)
  }

  fn get_ty(&self, name: &str_util::Name) -> Option<&TyInfo> {
    self.ty_env.get(name)
  }

  fn get_val(&self, name: &str_util::Name) -> Option<&ValInfo> {
    self.val_env.get(name)
  }

  fn append(&mut self, other: &mut Self) {
    self.str_env.extend(other.str_env.drain());
    self.ty_env.extend(other.ty_env.drain());
    self.val_env.extend(other.val_env.drain());
  }

  fn all_str(&self) -> Vec<&Env> {
    self.str_env.values().collect()
  }

  fn all_ty(&self) -> Vec<&TyInfo> {
    self.ty_env.values().collect()
  }

  fn all_val(&self) -> Vec<&ValInfo> {
    self.val_env.values().collect()
  }

  fn into_env(self) -> Env {
    self
  }
}

/// A wrapper around a stack of [`Env`]s. Is meant to act like an `Env` in most respects, but is
/// faster to `Clone`.
#[derive(Debug, Default, Clone)]
pub(crate) struct EnvStack(Vec<Arc<Env>>);

impl EnvStack {
  pub(crate) fn one(env: Env) -> Self {
    Self(vec![Arc::new(env)])
  }

  pub(crate) fn push(&mut self, other: Env) {
    self.0.push(Arc::new(other));
  }
}

impl EnvLike for EnvStack {
  fn get_str(&self, name: &str_util::Name) -> Option<&Env> {
    self.0.iter().rev().find_map(|env| env.str_env.get(name))
  }

  fn get_ty(&self, name: &str_util::Name) -> Option<&TyInfo> {
    self.0.iter().rev().find_map(|env| env.ty_env.get(name))
  }

  fn get_val(&self, name: &str_util::Name) -> Option<&ValInfo> {
    self.0.iter().rev().find_map(|env| env.val_env.get(name))
  }

  fn append(&mut self, other: &mut Env) {
    let mut env = Env::default();
    env.append(other);
    self.push(env);
  }

  fn all_str(&self) -> Vec<&Env> {
    let mut names = FxHashSet::<&str_util::Name>::default();
    self
      .0
      .iter()
      .rev()
      .flat_map(|env| env.str_env.iter())
      .filter_map(|(name, val)| names.insert(name).then_some(val))
      .collect()
  }

  fn all_ty(&self) -> Vec<&TyInfo> {
    let mut names = FxHashSet::<&str_util::Name>::default();
    self
      .0
      .iter()
      .rev()
      .flat_map(|env| env.ty_env.iter())
      .filter_map(|(name, val)| names.insert(name).then_some(val))
      .collect()
  }

  fn all_val(&self) -> Vec<&ValInfo> {
    let mut names = FxHashSet::<&str_util::Name>::default();
    self
      .0
      .iter()
      .rev()
      .flat_map(|env| env.val_env.iter())
      .filter_map(|(name, val)| names.insert(name).then_some(val))
      .collect()
  }

  fn into_env(mut self) -> Env {
    let mut env = Env::default();
    for mut other in self.0.drain(..) {
      env.append(Arc::make_mut(&mut other));
    }
    env
  }
}

/// Definition: Context
///
/// No need for the set of ty names as from the Definition; it seems to only be used to ensure a
/// type name does not escape its scope, and for that we use `Sym::generated_after`.
#[derive(Debug, Clone)]
pub(crate) struct Cx {
  pub(crate) env: EnvStack,
  /// the Definition has this as a set, but we have it as a mapping.
  ///
  /// this isn't really `ty_vars(C)` as in the definition, since it's just fixed ty vars.
  pub(crate) fixed: FxHashMap<sml_hir::TyVar, FixedTyVar>,
}

/// Definition: `TyNameSet`
pub(crate) type TyNameSet = FxHashSet<Sym>;

/// Definition: Sig
#[derive(Debug, Clone)]
pub(crate) struct Sig {
  pub(crate) ty_names: TyNameSet,
  pub(crate) env: Env,
}

/// Definition: `FunSig`
#[derive(Debug, Clone)]
pub(crate) struct FunSig {
  pub(crate) param: Sig,
  pub(crate) body_ty_names: TyNameSet,
  pub(crate) body_env: Env,
}

pub(crate) type SigEnv = FxHashMap<str_util::Name, Sig>;
pub(crate) type FunEnv = FxHashMap<str_util::Name, FunSig>;

/// Definition: Basis
#[derive(Debug, Default, Clone)]
pub(crate) struct Bs<E = EnvStack> {
  pub(crate) env: E,
  pub(crate) sig_env: Arc<SigEnv>,
  pub(crate) fun_env: Arc<FunEnv>,
}

impl Bs {
  pub(crate) fn as_cx(&self) -> Cx {
    Cx { env: self.env.clone(), fixed: FxHashMap::default() }
  }
}

impl<E: EnvLike> Bs<E> {
  pub(crate) fn as_mut_fun_env(&mut self) -> &mut FunEnv {
    Arc::make_mut(&mut self.fun_env)
  }

  pub(crate) fn as_mut_sig_env(&mut self) -> &mut SigEnv {
    Arc::make_mut(&mut self.sig_env)
  }

  pub(crate) fn append<E2: EnvLike>(&mut self, mut other: Bs<E2>) {
    self.as_mut_sig_env().extend(other.as_mut_sig_env().drain());
    self.as_mut_fun_env().extend(other.as_mut_fun_env().drain());
    self.env.append(&mut other.env.into_env());
  }
}

/// Information about meta type variables.
#[derive(Debug, Default, Clone)]
pub struct MetaVarInfo(FxHashMap<MetaTyVar, TyVarKind>);

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

#[derive(Debug)]
pub(crate) enum SubstEntry {
  /// We solved this meta var to a `Ty`.
  Solved(Ty),
  /// This meta var is a special type variable, into which only certain kinds of types can be
  /// substituted.
  Kind(TyVarKind),
}

#[derive(Debug, Default, Clone)]
pub(crate) struct FixedTyVars(BTreeMap<FixedTyVar, Option<BoundTyVar>>);

impl FixedTyVars {
  pub(crate) fn insert(&mut self, var: FixedTyVar) {
    assert!(self.0.insert(var, None).is_none());
  }

  /// the ordering is always the same.
  pub(crate) fn iter(&self) -> impl Iterator<Item = &FixedTyVar> + '_ {
    self.0.keys()
  }
}

/// generalizes the type in the type scheme.
///
/// replaces:
/// - any fixed vars from `fixed_vars`
/// - any meta vars not already solved by `subst` which were generated after the `mv_marker`
///
/// in the type with bound vars, and updates the type scheme to bind those vars.
///
/// panics if the type scheme already binds vars.
#[allow(clippy::needless_pass_by_value)]
pub(crate) fn generalize(
  mv_generalizer: MetaTyVarGeneralizer,
  subst: &Subst,
  fixed: FixedTyVars,
  ty_scheme: &mut TyScheme,
) -> Result<(), HasRecordMetaVars> {
  assert!(ty_scheme.bound_vars.is_empty());
  let mut meta = FxHashMap::<MetaTyVar, Option<BoundTyVar>>::default();
  // assigning 'ranks' to meta vars is all in service of allowing `meta` to be computed efficiently.
  // if we did not, we would have to traverse the whole `Env` to know what ty vars are present in
  // it, and subtract those vars from the vars in `ty_scheme.ty`.
  meta_vars(
    subst,
    &mut |x, _| {
      if mv_generalizer.is_generalizable(x) {
        meta.insert(x, None);
      }
    },
    &ty_scheme.ty,
  );
  let mut g = Generalizer {
    subst,
    fixed,
    meta,
    bound_vars: BoundTyVars::default(),
    has_record_meta_var: false,
  };
  g.go(&mut ty_scheme.ty);
  ty_scheme.bound_vars = g.bound_vars;
  if g.has_record_meta_var {
    Err(HasRecordMetaVars)
  } else {
    Ok(())
  }
}

/// a marker for when a type contained record meta vars.
#[derive(Debug)]
pub(crate) struct HasRecordMetaVars;

/// like [`generalize`], but:
///
/// - doesn't allow meta vars
/// - always generalizes exactly the given fixed vars, even if they don't appear in the
///   `ty_scheme.ty`
///
/// use this to:
///
/// - explicitly create a ty scheme with the written arity, e.g. to support phantom types.
/// - preserve the order of fixed type vars for the bound ty var binders.
pub(crate) fn generalize_fixed(mut fixed: FixedTyVars, ty_scheme: &mut TyScheme) {
  assert!(ty_scheme.bound_vars.is_empty());
  let mut bound_vars = Vec::with_capacity(fixed.0.len());
  for (idx, (fv, bv)) in fixed.0.iter_mut().enumerate() {
    assert!(bv.is_none());
    bound_vars.push(fv.ty_var.is_equality().then_some(TyVarKind::Equality));
    *bv = Some(BoundTyVar(idx));
  }
  let mut g = Generalizer {
    subst: &Subst::default(),
    fixed,
    meta: FxHashMap::default(),
    bound_vars: BoundTyVars(bound_vars),
    has_record_meta_var: false,
  };
  g.go(&mut ty_scheme.ty);
  ty_scheme.bound_vars = g.bound_vars;
  assert!(!g.has_record_meta_var, "there should be no meta vars at all, much less record ones");
}

pub(crate) fn meta_vars<F>(subst: &Subst, f: &mut F, ty: &Ty)
where
  F: FnMut(MetaTyVar, Option<&TyVarKind>),
{
  match ty {
    Ty::None | Ty::BoundVar(_) | Ty::FixedVar(_) => {}
    Ty::MetaVar(mv) => match subst.get(*mv) {
      None => f(*mv, None),
      Some(SubstEntry::Kind(k)) => f(*mv, Some(k)),
      Some(SubstEntry::Solved(ty)) => meta_vars(subst, f, ty),
    },
    Ty::Record(rows) => {
      for ty in rows.values() {
        meta_vars(subst, f, ty);
      }
    }
    Ty::Con(args, _) => {
      for ty in args.iter() {
        meta_vars(subst, f, ty);
      }
    }
    Ty::Fn(param, res) => {
      meta_vars(subst, f, param);
      meta_vars(subst, f, res);
    }
  }
}

struct Generalizer<'a> {
  subst: &'a Subst,
  fixed: FixedTyVars,
  meta: FxHashMap<MetaTyVar, Option<BoundTyVar>>,
  bound_vars: BoundTyVars,
  has_record_meta_var: bool,
}

impl<'a> Generalizer<'a> {
  fn go(&mut self, ty: &mut Ty) {
    match ty {
      Ty::None => {}
      Ty::BoundVar(_) => unreachable!("bound vars should be instantiated"),
      Ty::MetaVar(mv) => match self.subst.get(*mv) {
        None => handle_bv(
          self.meta.get_mut(mv),
          &mut self.bound_vars,
          &mut self.has_record_meta_var,
          None,
          ty,
        ),
        Some(entry) => match entry {
          SubstEntry::Solved(t) => {
            *ty = t.clone();
            self.go(ty);
          }
          SubstEntry::Kind(k) => handle_bv(
            self.meta.get_mut(mv),
            &mut self.bound_vars,
            &mut self.has_record_meta_var,
            Some(k.clone()),
            ty,
          ),
        },
      },
      Ty::FixedVar(fv) => handle_bv(
        self.fixed.0.get_mut(fv),
        &mut self.bound_vars,
        &mut self.has_record_meta_var,
        fv.ty_var.is_equality().then_some(TyVarKind::Equality),
        ty,
      ),
      Ty::Record(rows) => {
        for ty in rows.values_mut() {
          self.go(ty);
        }
      }
      Ty::Con(args, _) => {
        for ty in args.iter_mut() {
          self.go(ty);
        }
      }
      Ty::Fn(param, res) => {
        self.go(param);
        self.go(res);
      }
    }
  }
}

fn handle_bv(
  bv: Option<&mut Option<BoundTyVar>>,
  bound_vars: &mut BoundTyVars,
  has_record_meta_var: &mut bool,
  kind: Option<TyVarKind>,
  ty: &mut Ty,
) {
  let bv = match bv {
    Some(bv) => bv,
    None => return,
  };
  *ty = match bv {
    Some(bv) => Ty::BoundVar(bv.clone()),
    None => match kind {
      Some(TyVarKind::Overloaded(ov)) => match ov {
        Overload::Basic(b) => match b {
          BasicOverload::Int => Ty::INT,
          BasicOverload::Real => Ty::REAL,
          BasicOverload::Word => Ty::WORD,
          BasicOverload::String => Ty::STRING,
          BasicOverload::Char => Ty::CHAR,
        },
        // all composite overloads contain, and default to, int.
        Overload::Composite(_) => Ty::INT,
      },
      Some(TyVarKind::Record(_)) => {
        *has_record_meta_var = true;
        Ty::None
      }
      None | Some(TyVarKind::Equality) => {
        let new_bv = BoundTyVar(bound_vars.len());
        *bv = Some(new_bv.clone());
        bound_vars.0.push(kind);
        Ty::BoundVar(new_bv)
      }
    },
  };
}

/// A definition site.
#[derive(Debug, Clone, Copy)]
pub struct Def {
  /// The path.
  pub path: DefPath,
  /// The HIR index.
  pub idx: sml_hir::Idx,
}

/// A definition path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefPath {
  /// A regular path.
  Regular(paths::PathId),
  /// A std basis path.
  StdBasis(&'static str),
}
