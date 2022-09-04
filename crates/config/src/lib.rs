//! Configuration.

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

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
}

/// The workspace config.
#[derive(Debug, Deserialize)]
pub struct Workspace {
  /// The root group filename.
  pub root: Option<SmolStr>,
  /// Path vars, for expansion in MLB/CM paths.
  #[serde(rename = "path-vars")]
  pub path_vars: Option<FxHashMap<SmolStr, PathVar>>,
}

/// A path var setting.
#[derive(Debug, Deserialize)]
pub enum PathVar {
  /// A literal value.
  #[serde(rename = "value")]
  Value(SmolStr),
  /// A path, interpreted relative to the config file.
  #[serde(rename = "path")]
  Path(SmolStr),
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
#[derive(Debug, Default, Deserialize)]
pub struct Options {}
