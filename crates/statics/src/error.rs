use crate::pat_match::Pat;
use crate::types::{MetaTyVar, Overload, Syms, Ty};
use std::fmt;

/// A statics error.
#[derive(Debug)]
pub struct Error {
  pub(crate) idx: Idx,
  pub(crate) kind: ErrorKind,
}

impl Error {
  /// Returns the [`Idx`] for this error.
  pub fn idx(&self) -> Idx {
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

/// Something that can have a statics error.
#[derive(Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Idx {
  Exp(hir::la_arena::Idx<hir::Exp>),
  Pat(hir::la_arena::Idx<hir::Pat>),
  Ty(hir::la_arena::Idx<hir::Ty>),
  Dec(hir::la_arena::Idx<hir::Dec>),
}

impl From<hir::la_arena::Idx<hir::Exp>> for Idx {
  fn from(val: hir::la_arena::Idx<hir::Exp>) -> Self {
    Self::Exp(val)
  }
}

impl From<hir::la_arena::Idx<hir::Pat>> for Idx {
  fn from(val: hir::la_arena::Idx<hir::Pat>) -> Self {
    Self::Pat(val)
  }
}

impl From<hir::la_arena::Idx<hir::Ty>> for Idx {
  fn from(val: hir::la_arena::Idx<hir::Ty>) -> Self {
    Self::Ty(val)
  }
}

impl From<hir::la_arena::Idx<hir::Dec>> for Idx {
  fn from(val: hir::la_arena::Idx<hir::Dec>) -> Self {
    Self::Dec(val)
  }
}

#[derive(Debug)]
pub(crate) enum ErrorKind {
  Unimplemented,
  Undefined(Item, hir::Name),
  Duplicate(Item, hir::Name),
  Circularity(MetaTyVar, Ty),
  MismatchedTypes(Ty, Ty),
  OverloadMismatch(Overload, Ty),
  MissingField(hir::Lab, Ty),
  ExtraFields(Vec<hir::Lab>, Ty),
  DuplicateLab(hir::Lab),
  RealPat,
  UnreachablePattern,
  NonExhaustiveMatch(Vec<Pat>),
  NonExhaustiveBinding(Vec<Pat>),
  PatValIdStatus,
  PatMustNotHaveArg,
  PatMustHaveArg,
  InvalidAsPatName,
  TyNameEscape,
  ValRecExpNotFn,
  WrongNumTyArgs(usize, usize),
  ExnCopyNotExnIdStatus,
}

#[derive(Debug)]
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
      ErrorKind::Unimplemented => f.write_str("support for this construct not implemented"),
      ErrorKind::Undefined(item, name) => write!(f, "undefined {item}: {name}"),
      ErrorKind::Duplicate(item, name) => write!(f, "duplicate {item}: {name}"),
      ErrorKind::Circularity(mv, ty) => {
        write!(
          f,
          "circularity: {} appears in {}",
          mv,
          ty.display(self.syms)
        )
      }
      ErrorKind::MismatchedTypes(want, got) => write!(
        f,
        "mismatched types: expected {}, found {}",
        want.display(self.syms),
        got.display(self.syms)
      ),
      ErrorKind::OverloadMismatch(want, got) => write!(
        f,
        "mismatched types: expected {}, found {}",
        // TODO make this programmatic?
        match want {
          Overload::WordInt => "word or int",
          Overload::RealInt => "real or int",
          Overload::Num => "word, real, or int",
          Overload::NumTxt => "word, real, int, string, or char",
        },
        got.display(self.syms)
      ),
      ErrorKind::MissingField(lab, ty) => {
        write!(f, "missing label {} in {}", lab, ty.display(self.syms))
      }
      ErrorKind::ExtraFields(_, ty) => {
        write!(f, "extra fields in {}", ty.display(self.syms))
      }
      ErrorKind::DuplicateLab(lab) => write!(f, "duplicate label: {}", lab),
      ErrorKind::RealPat => f.write_str("real literal used as a pattern"),
      ErrorKind::UnreachablePattern => f.write_str("unreachable pattern"),
      ErrorKind::NonExhaustiveMatch(_) => f.write_str("non-exhaustive match"),
      ErrorKind::NonExhaustiveBinding(_) => f.write_str("non-exhaustive binding"),
      ErrorKind::PatValIdStatus => f.write_str("value binding used as a pattern"),
      ErrorKind::PatMustNotHaveArg => f.write_str("pattern must not have argument"),
      ErrorKind::PatMustHaveArg => f.write_str("pattern must have argument"),
      ErrorKind::InvalidAsPatName => f.write_str("invalid `as` pat name"),
      ErrorKind::TyNameEscape => f.write_str("type name escapes its scope"),
      ErrorKind::ValRecExpNotFn => f.write_str("the expression for a `val rec` was not a `fn`"),
      ErrorKind::WrongNumTyArgs(want, got) => write!(
        f,
        "wrong number of type arguments: expected {}, found {}",
        want, got
      ),
      ErrorKind::ExnCopyNotExnIdStatus => f.write_str("not an exception"),
    }
  }
}
