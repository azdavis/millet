//! Configuration options sent when the language server starts.
//!
//! The initialization options are a subset of the VS Code config, but rearranged and renamed
//! slightly. Consult the implementation of the VS Code extension to see what options are sent.
//! Additionally, consult the documentation for the VS Code configuration to see what types the
//! configuration options must be.

#![allow(missing_docs)]

use crate::tool::Tool;
use serde::Deserialize;

/// @sync(init-options)
#[derive(Debug, Default, Deserialize)]
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
pub struct DiagnosticsOptions {
  #[serde(default)]
  pub on_change: bool,
  #[serde(default)]
  pub more_info_hint: Tool,
  #[serde(default)]
  pub ignore: DiagnosticsIgnore,
}

#[derive(Debug, Default, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DiagnosticsIgnore {
  None,
  #[default]
  AfterSyntax,
  All,
}

#[derive(Debug, Default, Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum FormatEngine {
  #[default]
  None,
  Naive,
  Smlfmt,
}
