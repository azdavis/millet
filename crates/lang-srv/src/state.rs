//! The main mutable state of the language server.

use crate::cx::Cx;
use fast_hash::FxHashSet;
use lsp_types::Url;

pub(crate) struct Root {
  pub(crate) path: paths::CleanPathBuf,
  pub(crate) input: input::Input,
}

pub struct St {
  pub(crate) root: Option<Box<Root>>,
  pub(crate) cx: Cx,
  pub(crate) analysis: analysis::Analysis,
  pub(crate) has_diagnostics: FxHashSet<Url>,
}
