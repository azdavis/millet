//! High-level Intermediate Representation.
//!
//! An implementation MUST NOT depend on the various "flavors" for semantics, e.g. type check
//! differently based on the flavor. It MUST only be used for "niceties", e.g. to emit better error
//! messages.

#![allow(missing_docs)]

use la_arena::Arena;
use sml_path::Path;
use std::fmt;
use str_util::Name;

pub use la_arena;
pub use sml_lab::Lab;
pub use sml_scon::{Int, ParseIntError, SCon};

#[derive(Debug, Default)]
pub struct Arenas {
  pub str_dec: StrDecArena,
  pub str_exp: StrExpArena,
  pub sig_exp: SigExpArena,
  pub spec: SpecArena,
  pub exp: ExpArena,
  pub dec: DecArena,
  pub pat: PatArena,
  pub ty: TyArena,
}

macro_rules! mk_idx {
  ( $( $name:ident ),* $(,)? ) => {
    #[doc = "An index into an arena."]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Idx {
      $($name(la_arena::Idx<$name>),)*
    }

    $(
      impl From<la_arena::Idx<$name>> for Idx {
        fn from(val: la_arena::Idx<$name>) -> Self {
          Self::$name(val)
        }
      }
    )*
  };
}

mk_idx![StrDec, StrExp, SigExp, Spec, Dec, Exp, Pat, Ty];

pub type OptIdx<T> = Option<la_arena::Idx<T>>;
pub type Seq<T> = Vec<la_arena::Idx<T>>;

// modules //

/// Whether something used syntax sugar.
#[derive(Debug, Clone, Copy)]
pub enum Flavor {
  /// It used sugar.
  Sugared,
  /// It didn't.
  Plain,
}

#[derive(Debug)]
pub struct SigBind {
  pub name: Name,
  pub sig_exp: SigExpIdx,
}

#[derive(Debug)]
pub struct FunctorBind {
  pub functor_name: Name,
  pub param_name: Name,
  pub param_sig: SigExpIdx,
  pub body: StrExpIdx,
  pub flavor: Flavor,
}

pub type StrDecSeq = Seq<StrDec>;
pub type StrDecIdx = la_arena::Idx<StrDec>;
pub type StrDecArena = Arena<StrDec>;

/// @def(87) is handled by not distinguishing between top decs and str decs.
#[derive(Debug)]
pub enum StrDec {
  Dec(DecSeq),
  Structure(Vec<StrBind>),
  /// technically a top dec in the Definition.
  Signature(Vec<SigBind>),
  /// technically a top dec in the Definition.
  Functor(Vec<FunctorBind>),
  Local(StrDecSeq, StrDecSeq),
}

#[derive(Debug)]
pub struct StrBind {
  pub name: Name,
  pub str_exp: StrExpIdx,
}

pub type StrExpIdx = OptIdx<StrExp>;
pub type StrExpArena = Arena<StrExp>;

#[derive(Debug)]
pub enum StrExp {
  Struct(StrDecSeq),
  Path(Path),
  Ascription(StrExpIdx, Ascription, SigExpIdx),
  App(Name, StrExpIdx, Flavor),
  Let(StrDecSeq, StrExpIdx),
}

#[derive(Debug)]
pub enum Ascription {
  Transparent,
  Opaque,
}

pub type SigExpIdx = OptIdx<SigExp>;
pub type SigExpArena = Arena<SigExp>;

#[derive(Debug)]
pub enum SigExp {
  Spec(SpecSeq),
  Name(Name),
  Where(SigExpIdx, WhereKind),
}

#[derive(Debug)]
pub enum WhereKind {
  Type(Vec<TyVar>, Path, TyIdx),
  Structure(Path, Path),
}

pub type SpecSeq = Seq<Spec>;
pub type SpecIdx = la_arena::Idx<Spec>;
pub type SpecArena = Arena<Spec>;

#[derive(Debug)]
pub enum Spec {
  /// the `Vec<TyVar>` will always be empty from the source, but may be filled in implicitly.
  Val(Vec<TyVar>, Vec<ValDesc>),
  Ty(TyDesc),
  EqTy(TyDesc),
  /// The `TyBinds` are from `withtype`, since it's easier to process in statics than lower.
  Datatype(Vec<DatDesc>, Vec<TyBind>),
  DatatypeCopy(Name, Path),
  Exception(ExDesc),
  Str(StrDesc),
  Include(SigExpIdx),
  Sharing(SpecSeq, SharingKind, Vec<Path>),
}

#[derive(Debug, Clone, Copy)]
pub enum SharingKind {
  /// The non-derived form, `sharing type`.
  Regular,
  /// The derived form, `sharing`. Though this is a derived form, we represent it in HIR because
  /// lowering it requires non-trivial statics information.
  Derived,
}

#[derive(Debug)]
pub struct ValDesc {
  pub name: Name,
  pub ty: TyIdx,
}

#[derive(Debug)]
pub struct TyDesc {
  pub ty_vars: Vec<TyVar>,
  pub name: Name,
}

pub type DatDesc = DatBind;
pub type ConDesc = ConBind;

#[derive(Debug)]
pub struct ExDesc {
  pub name: Name,
  pub ty: Option<TyIdx>,
}

#[derive(Debug)]
pub struct StrDesc {
  pub name: Name,
  pub sig_exp: SigExpIdx,
}

// core //

pub type ExpIdx = OptIdx<Exp>;
pub type ExpArena = Arena<Exp>;

/// @def(7) is handled by having no distinction between atomic expressions and others here.
#[derive(Debug)]
pub enum Exp {
  Hole,
  SCon(SCon),
  Path(Path),
  Record(Vec<(Lab, ExpIdx)>),
  Let(DecSeq, ExpIdx),
  App(ExpIdx, ExpIdx),
  Handle(ExpIdx, Vec<Arm>),
  Raise(ExpIdx),
  Fn(Vec<Arm>, FnFlavor),
  Typed(ExpIdx, TyIdx, TypedFlavor),
  Vector(Vec<ExpIdx>),
}

#[derive(Debug, Clone, Copy)]
pub struct Arm {
  pub pat: PatIdx,
  pub exp: ExpIdx,
}

/// The original bit of syntax that got eventually lowered to stuff involving `fn`.
#[derive(Debug, Clone, Copy)]
pub enum FnFlavor {
  /// i.e. `andalso`/`orelse`. Lowers to `if`.
  BoolBinOp,
  /// Lowers to `case`.
  If,
  /// Lowers to `fn` applied to the head.
  Case,
  /// The `fn` that take in the `fun` arguments.
  FunArg,
  /// The `fn` that is lowered from the `case` on all of the `fun` arguments.
  FunCase { tuple: bool },
  /// Lowers to `fn` with a record pattern with a `...` pattern row.
  Selector,
  /// Lowers to `fun` and `if`.
  While,
  /// Lowers to successive `case`s.
  Seq,
  /// Lowers to itself!
  Fn,
}

/// The original bit of syntax that got eventually lowered to stuff involving `exp : ty`.
#[derive(Debug, Clone, Copy)]
pub enum TypedFlavor {
  Fun,
  Regular,
}

pub type DecSeq = Seq<Dec>;
pub type DecIdx = la_arena::Idx<Dec>;
pub type DecArena = Arena<Dec>;

#[derive(Debug)]
pub enum Dec {
  Val(Vec<TyVar>, Vec<ValBind>, ValFlavor),
  Ty(Vec<TyBind>),
  /// The `TyBinds` are from `withtype`, since it's easier to process in statics than lower.
  Datatype(Vec<DatBind>, Vec<TyBind>),
  DatatypeCopy(Name, Path),
  /// The `TyBinds` are from `withtype`, since it's easier to process in statics than lower.
  Abstype(Vec<DatBind>, Vec<TyBind>, DecSeq),
  Exception(Vec<ExBind>),
  Local(DecSeq, DecSeq),
  Open(Vec<Path>),
}

#[derive(Debug, Clone, Copy)]
pub struct ValBind {
  pub rec: bool,
  pub pat: PatIdx,
  pub exp: ExpIdx,
}

/// The original bit of syntax that got eventually lowered to stuff involving `val`.
#[derive(Debug, Clone, Copy)]
pub enum ValFlavor {
  /// A top-level expression `e` lowers to `val _ = e` (it should technically lower to `val it = e`
  /// but see the comment.)
  TopLevelExp,
  /// `do e` lowers to `val () = e`.
  Do,
  /// Lowers to `fun`.
  While,
  /// Lowers to `val rec` + a `case`.
  Fun,
  /// Lowers to itself.
  Val,
}

#[derive(Debug)]
pub struct TyBind {
  pub ty_vars: Vec<TyVar>,
  pub name: Name,
  pub ty: TyIdx,
}

#[derive(Debug)]
pub struct DatBind {
  pub ty_vars: Vec<TyVar>,
  pub name: Name,
  pub cons: Vec<ConBind>,
}

#[derive(Debug)]
pub struct ConBind {
  pub name: Name,
  pub ty: Option<TyIdx>,
}

#[derive(Debug)]
pub enum ExBind {
  New(Name, Option<TyIdx>),
  Copy(Name, Path),
}

pub type PatIdx = OptIdx<Pat>;
pub type PatArena = Arena<Pat>;

/// @def(40) is handled by having no distinction between atomic expressions and others here.
#[derive(Debug)]
pub enum Pat {
  Wild,
  SCon(SCon),
  Con(Path, Option<PatIdx>),
  Record {
    rows: Vec<(Lab, PatIdx)>,
    allows_other: bool,
  },
  Typed(PatIdx, TyIdx),
  /// the Definition defines as-pats as being a name, then an optional type annotation, then `as`,
  /// then a pat. however, we put the type annotation on the rhs pat.
  ///
  /// also note that we parse `<pat> as <pat>` but reject when lowering.
  As(Name, PatIdx),
  Or(OrPat),
  Vector(Vec<PatIdx>),
}

#[derive(Debug)]
pub struct OrPat {
  pub first: PatIdx,
  pub rest: Vec<PatIdx>,
}

impl OrPat {
  /// Returns an iterator of all the pats in order.
  pub fn all_pats(&self) -> impl Iterator<Item = PatIdx> {
    std::iter::once(self.first).chain(self.rest.iter().copied())
  }
}

pub type TyIdx = OptIdx<Ty>;
pub type TyArena = Arena<Ty>;

#[derive(Debug)]
pub enum Ty {
  Hole,
  Var(TyVar),
  Record(Vec<(Lab, TyIdx)>),
  Con(Vec<TyIdx>, Path),
  Fn(TyIdx, TyIdx),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum TyVarRepr {
  Name(Name),
  Unutterable(UnutterableTyVar),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVar(TyVarRepr);

impl TyVar {
  #[must_use]
  pub fn name(name: Name) -> Self {
    Self(TyVarRepr::Name(name))
  }

  #[must_use]
  pub fn unutterable(un: UnutterableTyVar) -> Self {
    Self(TyVarRepr::Unutterable(un))
  }

  #[must_use]
  pub fn is_equality(&self) -> bool {
    match &self.0 {
      TyVarRepr::Name(name) => name.as_str().as_bytes().get(1) == Some(&b'\''),
      TyVarRepr::Unutterable(un) => un.equality,
    }
  }

  #[must_use]
  pub fn into_name(self) -> Name {
    match self.0 {
      TyVarRepr::Name(name) => name,
      TyVarRepr::Unutterable(un) => Name::new(un.to_string()),
    }
  }
}

impl fmt::Display for TyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.0 {
      TyVarRepr::Name(name) => name.fmt(f),
      TyVarRepr::Unutterable(un) => un.fmt(f),
    }
  }
}

/// A type that when displayed could be a name for a **unutterable** type variable.
///
/// It will **not** be a name of an actual SML type variable that a user can type in real code.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[must_use]
pub struct UnutterableTyVar {
  equality: bool,
  idx: usize,
}

impl UnutterableTyVar {
  /// Returns a new one of these.
  pub fn new(equality: bool, idx: usize) -> Self {
    Self { equality, idx }
  }
}

impl fmt::Display for UnutterableTyVar {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let ticks = if self.equality { 2 } else { 1 };
    for c in std::iter::repeat('?').take(ticks).chain(idx_to_name(self.idx)) {
      write!(f, "{c}")?;
    }
    Ok(())
  }
}

fn idx_to_name(idx: usize) -> impl Iterator<Item = char> {
  let alpha = 26usize;
  let quot = idx / alpha;
  let rem = u8::try_from(idx % alpha).unwrap();
  let ch = char::from(b'a' + rem);
  std::iter::repeat(ch).take(quot + 1)
}
