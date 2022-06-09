use crate::top_dec;
use crate::util::{Cx, Lower};
use syntax::ast;

/// Does the conversion.
pub fn get(root: &ast::Root) -> Lower {
  let mut cx = Cx::default();
  let top_decs: Vec<_> = root
    .str_dec()
    .into_iter()
    .flat_map(|str_dec| str_dec.str_dec_in_seqs())
    .filter_map(|x| x.str_dec_one())
    .map(|top_dec| top_dec::get(&mut cx, top_dec))
    .collect();
  cx.finish(top_decs)
}
