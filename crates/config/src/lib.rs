//! Configuration.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use fast_hash::FxHashMap;
use serde::Deserialize;
use str_util::SmolStr;

/// The name of the config file.
pub const FILE_NAME: &str = "millet.toml";

/// The root config.
#[derive(Debug, Deserialize)]
pub struct Root {
  /// The version. Should be 1.
  pub version: u16,
  /// The workspace config.
  pub workspace: Option<Workspace>,
  /// The errors config.
  ///
  /// Cannot be set when `workspace.members` is set.
  pub errors: Option<FxHashMap<SmolStr, ErrorConfig>>,
}

/// The workspace config.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Workspace {
  /// The members, for containing other workspaces.
  ///
  /// Cannot be set when any other workspace setting is set.
  pub members: Option<Vec<SmolStr>>,
  /// The root group filename.
  ///
  /// Cannot be set when `members` is set.
  pub root: Option<SmolStr>,
  /// Path vars, for expansion in MLB/CM paths.
  ///
  /// Cannot be set when `members` is set.
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
pub struct ErrorConfig {
  /// The severity to display this error code at.
  pub severity: Option<Severity>,
}

/// A severity for an error.
#[derive(Debug, Deserialize)]
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

/// Optional settings for the server.
#[derive(Debug, Deserialize)]
pub struct Options {
  /// Show information about tokens on hover.
  pub show_token_hover: bool,
  /// Send diagnostics when file contents change before saving.
  pub diagnostics_on_change: bool,
  /// **WARNING: THE FORMATTER REWRITES YOUR CODE. IT IS HIGHLY EXPERIMENTAL. IT MAY IRREVOCABLY
  /// DESTROY SOME OR ALL OF YOUR CODE.**
  ///
  /// Naively format open SML files on save.
  pub format: bool,
}

impl Default for Options {
  fn default() -> Self {
    Self { show_token_hover: true, diagnostics_on_change: false, format: false }
  }
}
