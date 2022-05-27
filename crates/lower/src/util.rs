use syntax::ast::{self, AstPtr};

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

  pub(crate) fn str_dec_one(
    &mut self,
    val: hir::StrDec,
    _: AstPtr<ast::StrDecOne>,
  ) -> hir::StrDecIdx {
    Some(self.arenas.str_dec.alloc(val))
  }

  pub(crate) fn str_dec_seq(
    &mut self,
    val: Vec<hir::StrDecIdx>,
    _: AstPtr<ast::StrDec>,
  ) -> hir::StrDecIdx {
    Some(self.arenas.str_dec.alloc(hir::StrDec::Seq(val)))
  }

  pub(crate) fn str_exp(&mut self, val: hir::StrExp, _: AstPtr<ast::StrExp>) -> hir::StrExpIdx {
    Some(self.arenas.str_exp.alloc(val))
  }

  pub(crate) fn sig_exp(&mut self, val: hir::SigExp, _: AstPtr<ast::SigExp>) -> hir::SigExpIdx {
    Some(self.arenas.sig_exp.alloc(val))
  }

  pub(crate) fn spec_one(&mut self, val: hir::Spec, _: AstPtr<ast::SpecOne>) -> hir::SpecIdx {
    Some(self.arenas.spec.alloc(val))
  }

  pub(crate) fn spec_seq(&mut self, val: Vec<hir::SpecIdx>, _: AstPtr<ast::Spec>) -> hir::SpecIdx {
    Some(self.arenas.spec.alloc(hir::Spec::Seq(val)))
  }

  pub(crate) fn exp(&mut self, val: hir::Exp, _: AstPtr<ast::Exp>) -> hir::ExpIdx {
    Some(self.arenas.exp.alloc(val))
  }

  pub(crate) fn dec_one(&mut self, val: hir::Dec, _: AstPtr<ast::DecOne>) -> hir::DecIdx {
    Some(self.arenas.dec.alloc(val))
  }

  pub(crate) fn dec_in_exp(&mut self, val: hir::Dec, _: AstPtr<ast::Exp>) -> hir::DecIdx {
    Some(self.arenas.dec.alloc(val))
  }

  pub(crate) fn dec_seq(&mut self, val: Vec<hir::DecIdx>, _: AstPtr<ast::Dec>) -> hir::DecIdx {
    Some(self.arenas.dec.alloc(hir::Dec::Seq(val)))
  }

  pub(crate) fn pat(&mut self, val: hir::Pat, _: AstPtr<ast::Pat>) -> hir::PatIdx {
    Some(self.arenas.pat.alloc(val))
  }

  pub(crate) fn pat_in_exp(&mut self, val: hir::Pat, _: AstPtr<ast::Exp>) -> hir::PatIdx {
    Some(self.arenas.pat.alloc(val))
  }

  pub(crate) fn ty(&mut self, val: hir::Ty, _: AstPtr<ast::Ty>) -> hir::TyIdx {
    Some(self.arenas.ty.alloc(val))
  }
}
