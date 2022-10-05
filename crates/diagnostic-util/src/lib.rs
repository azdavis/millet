//! Utilities for diagnostics (colloquially, "errors") reported by Millet.

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

/// The url to go to for information about errors.
pub const ERRORS_URL: &str = "https://github.com/azdavis/millet/blob/main/docs/errors.md";

/// An error.
#[derive(Debug)]
pub struct Error {
  /// The range of the error.
  pub range: text_pos::Range,
  /// The message of the error.
  pub message: String,
  /// The error code.
  pub code: u16,
  /// The severity.
  pub severity: Severity,
}

/// The severity of this error.
#[derive(Debug, Clone, Copy)]
pub enum Severity {
  /// Error-level severity. The maximum.
  Error,
}
