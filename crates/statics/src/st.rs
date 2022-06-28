use crate::error::{Error, ErrorKind};
use crate::info::Info;
use crate::std_basis;
use crate::types::{Bs, Def, FixedTyVar, FixedTyVarGen, MetaTyVar, MetaTyVarGen, Subst, Syms};

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
  info: Info,
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
      info: Info::default(),
      syms,
    }
  }

  pub(crate) fn mode(&self) -> Mode {
    self.mode
  }

  pub(crate) fn def<I>(&self, idx: I) -> Option<Def>
  where
    I: Into<hir::Idx>,
  {
    self.mode().path().map(|path| Def {
      path,
      idx: idx.into(),
    })
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

  pub(crate) fn info(&mut self) -> &mut Info {
    &mut self.info
  }

  pub(crate) fn finish(self) -> (Syms, Vec<Error>, Subst, Info) {
    (self.syms, self.errors, self.subst, self.info)
  }
}

/// Static analysis.
///
/// Note that the `Default` impl will return a `Statics` with a `Syms` that contains only the most
/// basic types and values, like `int` and `>`.
#[derive(Debug, Clone)]
pub struct Statics {
  pub(crate) syms: Syms,
  pub(crate) bs: Bs,
}

impl Statics {
  /// Returns the symbols generated.
  pub fn syms(&self) -> &Syms {
    &self.syms
  }

  /// Turns this into a `Syms`.
  pub fn into_syms(self) -> Syms {
    self.syms
  }
}

impl Default for Statics {
  fn default() -> Self {
    std_basis::get()
  }
}

/// The mode for checking.
#[derive(Debug, Clone, Copy)]
pub enum Mode {
  /// Regular checking. The default.
  Regular(Option<paths::PathId>),
  /// Declaration-mode checking. Notably, ascription structure expressions will not check to see if
  /// they actually match the signature. This should only be used for special files, like ones that
  /// define the standard basis.
  Declaration,
}

impl Mode {
  pub(crate) fn is_regular(self) -> bool {
    matches!(self, Self::Regular(_))
  }

  fn path(self) -> Option<paths::PathId> {
    match self {
      Self::Regular(p) => p,
      Self::Declaration => None,
    }
  }
}
