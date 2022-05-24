use crate::error::{Error, ErrorKind};
use crate::types::{MetaTyVar, MetaTyVarGen, Subst, Syms, TyVars};

/// The state.
///
/// Usually I call this `Cx` but the Definition defines a 'Context' already.
///
/// Invariant: 'Grows' monotonically.
#[derive(Debug, Default)]
pub(crate) struct St {
  subst: Subst,
  errors: Vec<Error>,
  meta_gen: MetaTyVarGen,
  pub(crate) syms: Syms,
}

impl St {
  pub(crate) fn subst(&mut self) -> &mut Subst {
    &mut self.subst
  }

  pub(crate) fn err(&mut self, kind: ErrorKind) {
    self.errors.push(Error { kind })
  }

  pub(crate) fn gen_meta_var(&mut self) -> MetaTyVar {
    self.meta_gen.gen(false)
  }

  pub(crate) fn gen_from_ty_vars<'a>(
    &'a mut self,
    ty_vars: &'a TyVars,
  ) -> impl Iterator<Item = MetaTyVar> + 'a {
    self.meta_gen.gen_from_ty_vars(ty_vars)
  }

  pub(crate) fn finish(self) -> Vec<Error> {
    self.errors
  }
}
