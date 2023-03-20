//! Lowering a whole program.

use crate::util::{Lower, St};
use sml_syntax::ast;

/// Does the conversion.
#[must_use]
pub fn get(root: &ast::Root) -> Lower {
  let mut st = St::new(&());
  let idx = crate::dec::get_top_dec(&mut st, root.dec());
  st.finish(idx)
}
