//! Configuration.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

pub mod file;
pub mod init;
pub mod lang;
pub mod tool;

/// How many lines a diagnostic message may have.
#[derive(Debug, Default, Clone, Copy)]
pub enum DiagnosticLines {
  /// Error messages may not have newlines.
  #[default]
  One,
  /// Error messages may (or may not) have newlines.
  Many,
}
