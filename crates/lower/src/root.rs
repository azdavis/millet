use crate::top_dec;
use crate::util::{Cx, Lowered};
use syntax::ast;

/// Does the conversion.
pub fn get(root: ast::Root) -> Lowered {
  let mut cx = Cx::default();
  for top_dec in root.top_decs() {
    top_dec::get(&mut cx, top_dec);
  }
  Lowered {}
}
