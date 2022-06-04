use crate::pat_match::Pat;
use crate::types::{MetaTyVar, Overload, Syms, Ty};
use std::fmt;

/// A statics error.
#[derive(Debug)]
pub struct Error {
  pub(crate) idx: hir::Idx,
  pub(crate) kind: ErrorKind,
}

impl Error {
  /// Returns the [`hir::Idx`] for this error.
  pub fn idx(&self) -> hir::Idx {
    self.idx
  }

  /// Displays this error.
  pub fn display<'a>(&'a self, syms: &'a Syms) -> impl fmt::Display + 'a {
    ErrorKindDisplay {
      kind: &self.kind,
      syms,
    }
  }
}

#[derive(Debug)]
pub(crate) enum ErrorKind {
  Unsupported(&'static str),
  Undefined(Item, hir::Name),
  Duplicate(Item, hir::Name),
  Missing(Item, hir::Name),
  Extra(Item, hir::Name),
  Circularity(MetaTyVar, Ty),
  MismatchedTypes(Ty, Ty),
  OverloadMismatch(Overload, Ty, Ty),
  AppLhsNotFn(Ty),
  DuplicateLab(hir::Lab),
  RealPat,
  UnreachablePattern,
  NonExhaustiveMatch(Vec<Pat>),
  NonExhaustiveBinding(Vec<Pat>),
  PatValIdStatus,
  ConPatMustNotHaveArg,
  ConPatMustHaveArg,
  InvalidAsPatName(hir::Name),
  TyNameEscape,
  ValRecExpNotFn,
  WrongNumTyArgs(usize, usize),
  ExnCopyNotExnIdStatus,
  InvalidRebindName(hir::Name),
  PolymorphicExn,
  WrongIdStatus(hir::Name),
}

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) enum Item {
  Val,
  Ty,
  TyVar,
  Struct,
  Sig,
  Functor,
}

impl fmt::Display for Item {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Item::Val => f.write_str("value"),
      Item::Ty => f.write_str("type"),
      Item::TyVar => f.write_str("type variable"),
      Item::Struct => f.write_str("structure"),
      Item::Sig => f.write_str("signature"),
      Item::Functor => f.write_str("functor"),
    }
  }
}

struct ErrorKindDisplay<'a> {
  kind: &'a ErrorKind,
  syms: &'a Syms,
}

impl fmt::Display for ErrorKindDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.kind {
      ErrorKind::Unsupported(s) => write!(f, "unsupported language construct: {s}"),
      ErrorKind::Undefined(item, name) => write!(f, "undefined {item}: {name}"),
      ErrorKind::Duplicate(item, name) => write!(f, "duplicate {item}: {name}"),
      ErrorKind::Missing(item, name) => write!(f, "missing {item} required by signature: {name}"),
      ErrorKind::Extra(item, name) => write!(f, "extra {item} not present in signature: {name}"),
      ErrorKind::Circularity(_, ty) => {
        write!(f, "circularity: {}", ty.display(self.syms))
      }
      ErrorKind::MismatchedTypes(want, got) => write!(
        f,
        "mismatched types: expected {}, found {}",
        want.display(self.syms),
        got.display(self.syms)
      ),
      ErrorKind::OverloadMismatch(ov, want, got) => write!(
        f,
        "mismatched types: expected {} with {}, found {}",
        want.display(self.syms),
        // TODO make this programmatic?
        match ov {
          Overload::WordInt => "word or int",
          Overload::RealInt => "real or int",
          Overload::Num => "word, real, or int",
          Overload::NumTxt => "word, real, int, string, or char",
        },
        got.display(self.syms)
      ),
      ErrorKind::AppLhsNotFn(got) => write!(
        f,
        "expected a function type, got {}",
        got.display(self.syms)
      ),
      ErrorKind::DuplicateLab(lab) => write!(f, "duplicate label: {lab}"),
      ErrorKind::RealPat => f.write_str("real literal used as a pattern"),
      ErrorKind::UnreachablePattern => f.write_str("unreachable pattern"),
      ErrorKind::NonExhaustiveMatch(_) => f.write_str("non-exhaustive match"),
      ErrorKind::NonExhaustiveBinding(_) => f.write_str("non-exhaustive binding"),
      ErrorKind::PatValIdStatus => f.write_str("value binding used as a pattern"),
      ErrorKind::ConPatMustNotHaveArg => f.write_str("unexpected argument for constructor pattern"),
      ErrorKind::ConPatMustHaveArg => f.write_str("missing argument for constructor pattern"),
      ErrorKind::InvalidAsPatName(name) => write!(f, "invalid `as` pat name: {name}"),
      ErrorKind::TyNameEscape => f.write_str("type name escapes its scope"),
      ErrorKind::ValRecExpNotFn => f.write_str("the expression for a `val rec` was not a `fn`"),
      ErrorKind::WrongNumTyArgs(want, got) => write!(
        f,
        "wrong number of type arguments: expected {}, found {}",
        want, got
      ),
      ErrorKind::ExnCopyNotExnIdStatus => f.write_str("not an exception"),
      ErrorKind::InvalidRebindName(name) => write!(f, "cannot re-bind name: {name}"),
      ErrorKind::PolymorphicExn => f.write_str("cannot have a polymorphic `exception`"),
      ErrorKind::WrongIdStatus(name) => write!(f, "incompatible identifier statuses: {name}"),
    }
  }
}
