//! Configuration.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use fast_hash::FxHashMap;
use serde::Deserialize;
use str_util::SmolStr;

/// The name of the config file.
pub const FILE_NAME: &str = "millet.toml";

/// The root config.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Root {
  /// The version. Should be 1.
  pub version: u16,
  /// The workspace config.
  pub workspace: Option<Workspace>,
  /// The diagnostics config.
  pub diagnostics: Option<FxHashMap<SmolStr, ErrorConfig>>,
}

/// The workspace config.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Workspace {
  /// The root group filename.
  pub root: Option<SmolStr>,
  /// Path vars, for expansion in MLB/CM paths.
  pub path_vars: Option<FxHashMap<SmolStr, PathVar>>,
}

/// A path var setting.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum PathVar {
  /// A literal value.
  Value(SmolStr),
  /// A path, interpreted relative to the config file.
  Path(SmolStr),
}

/// Configuration for an error code.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ErrorConfig {
  /// The severity to display this error code at.
  pub severity: Option<Severity>,
}

/// A severity for an error.
#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Severity {
  /// Ignore this error.
  Ignore,
  /// Warning.
  Warning,
  /// Error. The maximum.
  Error,
}

/// How many lines an error message may have.
#[derive(Debug, Clone, Copy)]
pub enum ErrorLines {
  /// Error messages may not have newlines.
  One,
  /// Error messages may (or may not) have newlines.
  Many,
}

/// What diagnostics to send per file.
#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DiagnosticsFilter {
  /// No filter, i.e. available diagnostics are sent.
  None,
  /// If there are syntax diagnostics (lex, parse, etc), send only those. Do not send e.g. statics
  /// diagnostics.
  Syntax,
}

/// Optional settings for the server.
#[derive(Debug, Deserialize)]
#[allow(missing_docs)]
#[allow(clippy::struct_excessive_bools)]
pub struct Options {
  pub show_token_hover: bool,
  pub diagnostics_on_change: bool,
  pub diagnostics_filter: DiagnosticsFilter,
  pub diagnostics_more_info_hint: bool,
  pub format: bool,
  /// TODO(equality-checks) remove
  pub equality_checks: bool,
}

impl Default for Options {
  fn default() -> Self {
    Self {
      show_token_hover: true,
      diagnostics_on_change: false,
      diagnostics_filter: DiagnosticsFilter::Syntax,
      diagnostics_more_info_hint: true,
      format: false,
      equality_checks: true,
    }
  }
}
