//! Configuration.

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
