use crate::pat_match::Pat;
use crate::types::{MetaTyVar, Ty};

#[derive(Debug)]
pub(crate) enum Error {
  Circularity(MetaTyVar, Ty),
  MismatchedTypes(Ty, Ty),
  MissingField(hir::Lab, Ty),
  ExtraFields(Vec<hir::Lab>, Ty),
  DuplicateLab(hir::Lab),
  RealPat,
  UnreachablePattern(hir::PatIdx),
  NonExhaustiveMatch(Vec<Pat>),
}
