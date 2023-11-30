//! Lowering a whole program.

use crate::util::{Lower, St};
use sml_syntax::ast;

/// Does the conversion.
#[must_use]
pub fn get(lang: &config::lang::Language, file_kind: sml_file::Kind, root: &ast::Root) -> Lower {
  let mut st = St::new(lang, file_kind);
  let idx = crate::dec::get_top_dec(&mut st, root);
  st.finish(idx)
}
