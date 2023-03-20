//! Configuration.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

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
  pub workspace: Option<WorkspaceConfig>,
  /// The diagnostics config.
  pub diagnostics: Option<FxHashMap<SmolStr, ErrorConfig>>,
}

/// The workspace config.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct WorkspaceConfig {
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
  /// A path, interpreted relative to the workspace root file.
  WorkspacePath(SmolStr),
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
pub enum DiagnosticsIgnore {
  /// If there are syntax diagnostics (lex error, parse error, etc), send only those, and ignore
  /// e.g. statics diagnostics.
  AfterSyntax,
  /// All diagnostics are filtered out, i.e. no diagnostics are sent.
  All,
}

/// How to format open SML files on save.
#[derive(Debug, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum FormatEngine {
  /// Naive formatting.
  Naive,
  /// Formatting provided by [`smlfmt`](https://github.com/shwestrick/smlfmt).
  Smlfmt,
}

/// Optional settings for the server.
#[derive(Debug, Deserialize)]
#[allow(missing_docs)]
#[allow(clippy::struct_excessive_bools)]
pub struct Options {
  pub show_token_hover: bool,
  pub diagnostics_on_change: bool,
  pub diagnostics_ignore: Option<DiagnosticsIgnore>,
  pub diagnostics_more_info_hint: bool,
  pub format: Option<FormatEngine>,
}

impl Default for Options {
  fn default() -> Self {
    Self {
      show_token_hover: true,
      diagnostics_on_change: false,
      diagnostics_ignore: Some(DiagnosticsIgnore::AfterSyntax),
      diagnostics_more_info_hint: true,
      format: None,
    }
  }
}
