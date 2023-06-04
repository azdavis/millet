//! Configuration options sent when the language server starts.

use crate::tool::Tool;
use serde::Deserialize;

/// Settings for the server.
//
/// @sync(init-options)
#[derive(Debug, Default, Deserialize)]
#[allow(missing_docs)]
pub struct Options {
  #[serde(default)]
  pub token_hover: Tool,
  #[serde(default)]
  pub fs_watcher: Tool,
  #[serde(default)]
  pub format: Option<FormatEngine>,
  #[serde(default)]
  pub diagnostics: DiagnosticsOptions,
}

#[derive(Debug, Deserialize)]
#[allow(missing_docs)]
pub struct DiagnosticsOptions {
  pub on_change: bool,
  pub more_info_hint: bool,
  pub ignore: Option<DiagnosticsIgnore>,
}

impl Default for DiagnosticsOptions {
  fn default() -> Self {
    Self { on_change: false, more_info_hint: true, ignore: Some(DiagnosticsIgnore::AfterSyntax) }
  }
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
