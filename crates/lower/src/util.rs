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
  arenas: hir::Arenas,
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

  pub(crate) fn finish(self) -> hir::Arenas {
    self.arenas
  }

  // TODO require having an ast ptr?

  pub(crate) fn str_dec(&mut self, val: hir::StrDec) -> hir::StrDecIdx {
    Some(self.arenas.str_dec.alloc(val))
  }

  pub(crate) fn str_exp(&mut self, val: hir::StrExp) -> hir::StrExpIdx {
    Some(self.arenas.str_exp.alloc(val))
  }

  pub(crate) fn sig_exp(&mut self, val: hir::SigExp) -> hir::SigExpIdx {
    Some(self.arenas.sig_exp.alloc(val))
  }

  pub(crate) fn spec(&mut self, val: hir::Spec) -> hir::SpecIdx {
    Some(self.arenas.spec.alloc(val))
  }

  pub(crate) fn exp(&mut self, val: hir::Exp) -> hir::ExpIdx {
    Some(self.arenas.exp.alloc(val))
  }

  pub(crate) fn dec(&mut self, val: hir::Dec) -> hir::DecIdx {
    Some(self.arenas.dec.alloc(val))
  }

  pub(crate) fn pat(&mut self, val: hir::Pat) -> hir::PatIdx {
    Some(self.arenas.pat.alloc(val))
  }

  pub(crate) fn ty(&mut self, val: hir::Ty) -> hir::TyIdx {
    Some(self.arenas.ty.alloc(val))
  }
}
