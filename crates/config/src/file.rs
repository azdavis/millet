//! Configuration stored in a config file.

use crate::tool::Tool;
use fast_hash::FxHashMap;
use serde::Deserialize;
use str_util::SmolStr;

/// The path of the config file.
pub const PATH: &str = "millet.toml";

/// The root config.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Root {
  /// The version. Should be 1.
  pub version: u16,
  /// The workspace config.
  #[serde(default)]
  pub workspace: Workspace,
  /// The diagnostics config.
  #[serde(default)]
  pub diagnostics: FxHashMap<SmolStr, Diagnostic>,
  /// The language config.
  #[serde(default)]
  pub language: Language,
}

/// The workspace config.
#[derive(Debug, Default, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Workspace {
  /// The root group filename.
  pub root: Option<SmolStr>,
  /// Path vars, for expansion in MLB/CM paths.
  #[serde(default)]
  pub path_vars: FxHashMap<SmolStr, PathVar>,
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
  pub severity: Severity,
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

/// Configuration for the language.
#[derive(Debug, Default, Clone, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Language {
  /// Whether fixity declarations can take effect across files.
  #[serde(default)]
  pub fixity_across_files: bool,
  /// Configuration for declarations.
  #[serde(default)]
  pub dec: Dec,
  /// Configuration for expressions.
  #[serde(default)]
  pub exp: Exp,
  /// Configuration for values.
  #[serde(default)]
  pub val: FxHashMap<SmolStr, bool>,
  /// Configuration for structures.
  #[serde(default)]
  pub structure: FxHashMap<SmolStr, bool>,
}

/// Configuration for declarations.
#[derive(Debug, Default, Clone, Deserialize)]
#[allow(missing_docs)]
pub struct Dec {
  #[serde(default)]
  pub val: Tool,
  #[serde(default)]
  pub fun: Tool,
  #[serde(default, rename = "type")]
  pub type_: Tool,
  #[serde(default)]
  pub datatype: Tool,
  #[serde(default, rename = "datatype-copy")]
  pub datatype_copy: Tool,
  #[serde(default)]
  pub exception: Tool,
  #[serde(default)]
  pub open: Tool,
  #[serde(default)]
  pub fixity: Tool,
  #[serde(default)]
  pub local: Tool,
  #[serde(default)]
  pub structure: Tool,
  #[serde(default)]
  pub signature: Tool,
  #[serde(default)]
  pub functor: Tool,
  #[serde(default)]
  pub exp: Tool,
  #[serde(default)]
  pub include: Tool,
}

/// Configuration for expressions.
#[derive(Debug, Default, Clone, Deserialize)]
#[allow(missing_docs)]
pub struct Exp {
  #[serde(default, rename = "int-lit")]
  pub int_lit: Tool,
  #[serde(default, rename = "real-lit")]
  pub real_lit: Tool,
  #[serde(default, rename = "word-lit")]
  pub word_lit: Tool,
  #[serde(default, rename = "char-lit")]
  pub char_lit: Tool,
  #[serde(default, rename = "string-lit")]
  pub string_lit: Tool,
  #[serde(default)]
  pub path: Tool,
  #[serde(default)]
  pub record: Tool,
  #[serde(default)]
  pub selector: Tool,
  #[serde(default)]
  pub paren: Tool,
  #[serde(default)]
  pub tuple: Tool,
  #[serde(default)]
  pub list: Tool,
  #[serde(default)]
  pub seq: Tool,
  #[serde(default, rename = "let")]
  pub let_: Tool,
  #[serde(default)]
  pub app: Tool,
  #[serde(default)]
  pub infix: Tool,
  #[serde(default)]
  pub typed: Tool,
  #[serde(default)]
  pub andalso: Tool,
  #[serde(default)]
  pub orelse: Tool,
  #[serde(default)]
  pub handle: Tool,
  #[serde(default)]
  pub raise: Tool,
  #[serde(default, rename = "if")]
  pub if_: Tool,
  #[serde(default, rename = "while")]
  pub while_: Tool,
  #[serde(default)]
  pub case: Tool,
  #[serde(default, rename = "fn")]
  pub fn_: Tool,
}
