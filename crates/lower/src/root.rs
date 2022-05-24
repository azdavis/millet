use crate::top_dec;
use crate::util::{Cx, Lower};
use syntax::ast;

/// Does the conversion.
pub fn get(root: ast::Root) -> Lower {
  let mut cx = Cx::default();
  let top_decs: Vec<_> = root
    .top_decs()
    .map(|top_dec| top_dec::get(&mut cx, top_dec))
    .collect();
  Lower {
    arenas: cx.arenas,
    top_decs,
  }
}
