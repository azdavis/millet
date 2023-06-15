//! The main mutable state of the language server.

use crate::cx::Cx;
use fast_hash::FxHashSet;
use lsp_types::Url;

pub(crate) enum Mode {
  /// We have a workspace root.
  Root(Box<Root>),
  /// We have no workspace root.
  NoRoot,
}

pub(crate) struct Root {
  pub(crate) path: paths::CanonicalPathBuf,
  pub(crate) input: input::Input,
}

pub struct St {
  pub(crate) mode: Mode,
  pub(crate) cx: Cx,
  pub(crate) analysis: analysis::Analysis,
  pub(crate) has_diagnostics: FxHashSet<Url>,
}
