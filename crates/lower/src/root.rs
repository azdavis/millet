use crate::top_dec::get as get_top_dec;
use crate::util::{Cx, Lowered};
use syntax::ast::Root;

/// Does the conversion.
pub fn get(root: Root) -> Lowered {
  let mut cx = Cx::default();
  for top_dec in root.top_decs() {
    get_top_dec(&mut cx, top_dec);
  }
  Lowered {}
}
