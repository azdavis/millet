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
  pub format: FormatEngine,
  #[serde(default)]
  pub diagnostics: DiagnosticsOptions,
}

#[derive(Debug, Default, Deserialize)]
#[allow(missing_docs)]
pub struct DiagnosticsOptions {
  #[serde(default)]
  pub on_change: bool,
  #[serde(default)]
  pub more_info_hint: Tool,
  #[serde(default)]
  pub ignore: DiagnosticsIgnore,
}

/// What diagnostics to send per file.
#[derive(Debug, Default, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DiagnosticsIgnore {
  /// Ignore no diagnostics, i.e. send all diagnostics.
  None,
  /// If there are syntax diagnostics (lex error, parse error, etc), send only those, and ignore
  /// e.g. statics diagnostics.
  #[default]
  AfterSyntax,
  /// All diagnostics are filtered out, i.e. no diagnostics are sent.
  All,
}

/// How to format open SML files on save.
#[derive(Debug, Default, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum FormatEngine {
  /// No formatting.
  #[default]
  None,
  /// Naive formatting.
  Naive,
  /// Formatting provided by [`smlfmt`](https://github.com/shwestrick/smlfmt).
  Smlfmt,
}
