use crate::error::Error;
use crate::types::{MetaTyVar, MetaTyVarGen, Syms, Ty, TyVars};
use fast_hash::FxHashMap;

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
  syms: Syms,
}

impl St {
  pub(crate) fn subst(&mut self) -> &mut Subst {
    &mut self.subst
  }

  pub(crate) fn err(&mut self, error: Error) {
    self.errors.push(error)
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

  pub(crate) fn take_syms(&mut self) -> Syms {
    std::mem::take(&mut self.syms)
  }

  pub(crate) fn set_syms(&mut self, syms: Syms) {
    assert!(self.syms.is_empty());
    self.syms = syms;
  }
}

/// A mapping from [`MetaTyVar`]s to [`Ty`]s.
///
/// Invariant: Mappings are never removed.
#[derive(Debug, Default)]
pub(crate) struct Subst {
  map: FxHashMap<MetaTyVar, Ty>,
}

impl Subst {
  /// Panics if there was already an assignment for this [`MetaTyVar`].
  pub(crate) fn insert(&mut self, mv: MetaTyVar, ty: Ty) {
    assert!(self.map.insert(mv, ty).is_none())
  }

  pub(crate) fn get(&self, mv: &MetaTyVar) -> Option<&Ty> {
    self.map.get(mv)
  }
}
