use crate::error::{Error, ErrorKind};
use crate::std_basis;
use crate::types::{Bs, FixedTyVar, FixedTyVarGen, MetaTyVar, MetaTyVarGen, Subst, Syms};

/// The state.
///
/// Usually I call this `Cx` but the Definition defines a 'Context' already.
///
/// Invariant: 'Grows' monotonically.
#[derive(Debug)]
pub(crate) struct St {
  mode: Mode,
  subst: Subst,
  errors: Vec<Error>,
  meta_gen: MetaTyVarGen,
  fixed_gen: FixedTyVarGen,
  pub(crate) syms: Syms,
}

impl St {
  pub(crate) fn new(mode: Mode, syms: Syms) -> Self {
    Self {
      mode,
      subst: Subst::default(),
      errors: Vec::new(),
      meta_gen: MetaTyVarGen::default(),
      fixed_gen: FixedTyVarGen::default(),
      syms,
    }
  }

  pub(crate) fn mode(&self) -> Mode {
    self.mode
  }

  pub(crate) fn subst(&mut self) -> &mut Subst {
    &mut self.subst
  }

  pub(crate) fn err<I>(&mut self, idx: I, kind: ErrorKind)
  where
    I: Into<hir::Idx>,
  {
    self.errors.push(Error {
      idx: idx.into(),
      kind,
    })
  }

  pub(crate) fn gen_meta_var(&mut self) -> MetaTyVar {
    self.meta_gen.gen()
  }

  pub(crate) fn gen_fixed_var(&mut self, ty_var: hir::TyVar) -> FixedTyVar {
    self.fixed_gen.gen(ty_var)
  }

  pub(crate) fn finish(self) -> (Syms, Vec<Error>) {
    (self.syms, self.errors)
  }
}

/// Static analysis.
#[derive(Debug)]
pub struct Statics {
  /// The symbols generated.
  pub syms: Syms,
  /// The errors encountered.
  pub errors: Vec<Error>,
  /// The basis of the whole program.
  pub(crate) bs: Bs,
}

impl Default for Statics {
  fn default() -> Self {
    let (syms, bs) = std_basis::get();
    Self {
      syms,
      bs,
      errors: Vec::new(),
    }
  }
}

/// The mode for checking.
#[derive(Debug, Clone, Copy)]
pub enum Mode {
  /// Regular checking. The default.
  Regular,
  /// Declaration-mode checking. Notably, ascription structure expressions will not check to see if
  /// they actually match the signature. This should only be used for special files, like ones that
  /// define the standard basis.
  Declaration,
}

impl Default for Mode {
  fn default() -> Self {
    Self::Regular
  }
}
