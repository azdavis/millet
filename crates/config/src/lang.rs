//! Parsed configuration for the language

use crate::file;

/// Parsed configuration for the language.
#[derive(Debug, Default, Clone)]
pub struct Language {
  /// Configuration for declarations.
  pub dec: file::Dec,
  /// Configuration for expressions.
  pub exp: file::Exp,
}
