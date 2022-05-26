use crate::pat_match::Pat;
use crate::types::{MetaTyVar, Syms, Ty};
use std::fmt;

/// A statics error.
#[derive(Debug)]
pub struct Error {
  pub(crate) kind: ErrorKind,
}

impl Error {
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
  Unimplemented,
  Undefined,
  Redefined,
  Circularity(MetaTyVar, Ty),
  MismatchedTypes(Ty, Ty),
  MissingField(hir::Lab, Ty),
  ExtraFields(Vec<hir::Lab>, Ty),
  DuplicateLab(hir::Lab),
  RealPat,
  UnreachablePattern(hir::PatIdx),
  NonExhaustiveMatch(Vec<Pat>),
  NonExhaustiveBinding(Vec<Pat>),
  PatValIdStatus,
  PatMustNotHaveArg,
  PatMustHaveArg,
  InvalidAsPatName,
  TyNameEscape,
  ValRecExpNotFn,
  WrongNumTyArgs(usize, usize),
}

struct ErrorKindDisplay<'a> {
  kind: &'a ErrorKind,
  syms: &'a Syms,
}

impl fmt::Display for ErrorKindDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.kind {
      ErrorKind::Unimplemented => f.write_str("support for this construct not implemented"),
      ErrorKind::Undefined => f.write_str("undefined"),
      ErrorKind::Redefined => f.write_str("redefined"),
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
      ErrorKind::MissingField(lab, ty) => {
        write!(f, "missing label {} in {}", lab, ty.display(self.syms))
      }
      ErrorKind::ExtraFields(_, ty) => {
        write!(f, "extra fields in {}", ty.display(self.syms))
      }
      ErrorKind::DuplicateLab(lab) => write!(f, "duplicate label {}", lab),
      ErrorKind::RealPat => f.write_str("real literal used as a pattern"),
      ErrorKind::UnreachablePattern(_) => f.write_str("unreachable pattern"),
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
    }
  }
}
