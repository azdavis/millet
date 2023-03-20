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
  /// Configuration for expressions.
  #[serde(default)]
  pub exp: Exp,
}

/// Configuration for expressions.
#[derive(Debug, Default, Clone, Deserialize)]
#[allow(clippy::struct_excessive_bools)]
#[allow(missing_docs)]
pub struct Exp {
  #[serde(rename = "int-lit", default)]
  pub int_lit: Tool,
  #[serde(rename = "real-lit", default)]
  pub real_lit: Tool,
  #[serde(rename = "word-lit", default)]
  pub word_lit: Tool,
  #[serde(rename = "char-lit", default)]
  pub char_lit: Tool,
  #[serde(rename = "string-lit", default)]
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
  #[serde(rename = "let", default)]
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
  #[serde(rename = "if", default)]
  pub if_: Tool,
  #[serde(rename = "while", default)]
  pub while_: Tool,
  #[serde(default)]
  pub case: Tool,
  #[serde(rename = "fn", default)]
  pub fn_: Tool,
}

/// A default-`true` `bool`.
#[derive(Debug, Clone, Copy, Deserialize)]
pub struct Tool(pub bool);

impl Default for Tool {
  fn default() -> Self {
    Self(true)
  }
}

impl std::ops::Not for Tool {
  type Output = bool;
  fn not(self) -> Self::Output {
    !self.0
  }
}
