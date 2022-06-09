use crate::top_dec;
use crate::util::{Cx, Lower};
use syntax::ast;

/// Does the conversion.
pub fn get(root: &ast::Root) -> Lower {
  let mut cx = Cx::default();
  let top_decs: Vec<_> = root
    .top_dec_in_seqs()
    .filter_map(|top_dec| Some(top_dec::get(&mut cx, top_dec.top_dec()?)))
    .collect();
  cx.finish(top_decs)
}
