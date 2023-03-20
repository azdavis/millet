//! Configuration options sent when the language server starts.

use serde::Deserialize;

/// Settings for the server.
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
