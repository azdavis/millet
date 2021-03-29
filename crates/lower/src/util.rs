use hir::{Arenas, Name};

/// Pointers between the AST and the HIR.
#[derive(Debug)]
pub struct Ptrs {}

/// The result of lowering.
#[derive(Debug)]
pub struct Lowered {}

#[derive(Debug, Default)]
pub(crate) struct Cx {
  pub(crate) arenas: Arenas,
  fresh_idx: u32,
}

impl Cx {
  pub(crate) fn fresh(&mut self) -> Name {
    let ret = Name::new(self.fresh_idx.to_string());
    self.fresh_idx += 1;
    ret
  }
}
