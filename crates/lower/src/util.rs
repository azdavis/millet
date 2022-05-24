/// Pointers between the AST and the HIR.
#[derive(Debug)]
pub struct Ptrs {
  // TODO
}

/// The result of lowering.
#[derive(Debug)]
pub struct Lower {
  /// The arenas.
  pub arenas: hir::Arenas,
  /// The top-level declarations, in order.
  pub top_decs: Vec<hir::TopDec>,
}

#[derive(Debug, Default)]
pub(crate) struct Cx {
  /// TODO put this behind mutator methods that require having an ast ptr?
  pub(crate) arenas: hir::Arenas,
  fresh_idx: u32,
}

impl Cx {
  /// Returns a `Name` that is both:
  /// - not writeable in user code, and will thus not collide with any identifiers in user code;
  /// - distinct from all other `Name`s returned from this thus far, and will thus not collide
  ///   with any of those.
  pub(crate) fn fresh(&mut self) -> hir::Name {
    let ret = hir::Name::new(self.fresh_idx.to_string());
    self.fresh_idx += 1;
    ret
  }
}
