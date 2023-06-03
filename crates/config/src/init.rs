//! Configuration options sent when the language server starts.

use serde::Deserialize;

/// Settings for the server.
//
/// @sync(init-options)
#[derive(Debug, Deserialize)]
#[allow(missing_docs)]
pub struct Options {
  pub token_hover: bool,
  pub fs_watcher: bool,
  pub format: Option<FormatEngine>,
  pub diagnostics: DiagnosticsOptions,
}

impl Default for Options {
  fn default() -> Self {
    Self {
      token_hover: true,
      fs_watcher: true,
      format: None,
      diagnostics: DiagnosticsOptions::default(),
    }
  }
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
