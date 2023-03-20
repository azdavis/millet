//! Lowering a whole program.

use crate::util::{Lower, St};
use sml_syntax::ast;

/// Does the conversion.
#[must_use]
pub fn get(lang: &config::file::Language, root: &ast::Root) -> Lower {
  let mut st = St::new(lang);
  let idx = crate::dec::get_top_dec(&mut st, root.dec());
  st.finish(idx)
}
