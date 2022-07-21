use crate::top_dec;
use crate::util::{Cx, Lower};
use syntax::ast;

/// Does the conversion.
pub fn get(root: &ast::Root) -> Lower {
  let mut cx = Cx::default();
  let idx = top_dec::get_str_dec(&mut cx, root.str_dec());
  cx.finish(idx)
}
