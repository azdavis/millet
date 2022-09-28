//! The common error type.

use text_pos::Range;

/// An error.
#[derive(Debug)]
pub struct Error {
  /// The range of the error.
  pub range: Range,
  /// The message of the error.
  pub message: String,
  /// The error code.
  pub code: u16,
}
