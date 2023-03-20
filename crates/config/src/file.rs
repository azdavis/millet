//! Configuration stored in a config file.

use fast_hash::FxHashMap;
use serde::Deserialize;
use str_util::SmolStr;

/// The name of the config file.
pub const NAME: &str = "millet.toml";

/// The root config.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Root {
  /// The version. Should be 1.
  pub version: u16,
  /// The workspace config.
  pub workspace: Option<Workspace>,
  /// The diagnostics config.
  pub diagnostics: Option<FxHashMap<SmolStr, Diagnostic>>,
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
  /// A path, interpreted relative to the workspace root file.
  WorkspacePath(SmolStr),
}

/// Configuration for an error code.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Diagnostic {
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
