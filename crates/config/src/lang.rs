//! Parsed configuration for the language.

use crate::file;
use fast_hash::FxHashSet;

/// Parsed configuration for the language.
#[derive(Debug, Default, Clone)]
pub struct Language {
  /// Configuration for declarations.
  pub dec: file::Dec,
  /// Configuration for expressions.
  pub exp: file::Exp,
  /// Disallowed value paths.
  pub val: FxHashSet<sml_path::Path>,
}