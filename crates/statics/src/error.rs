use crate::pat_match::Pat;
use crate::types::{MetaTyVar, Ty};

#[derive(Debug)]
pub struct Error {
  pub(crate) kind: ErrorKind,
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
}
