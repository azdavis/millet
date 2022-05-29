use crate::error::{Error, ErrorKind, Idx};
use crate::types::{
  BoundTyVars, FixedTyVar, FixedTyVarGen, MetaTyVar, MetaTyVarGen, Subst, SubstEntry, Syms, Ty,
};
use drop_bomb::DropBomb;

/// The state.
///
/// Usually I call this `Cx` but the Definition defines a 'Context' already.
///
/// Invariant: 'Grows' monotonically.
#[derive(Debug, Default)]
pub struct St {
  subst: Subst,
  errors: Vec<Error>,
  meta_gen: MetaTyVarGen,
  fixed_gen: FixedTyVarGen,
  pub(crate) syms: Syms,
}

impl St {
  pub(crate) fn subst(&mut self) -> &mut Subst {
    &mut self.subst
  }

  pub(crate) fn err<I>(&mut self, idx: I, kind: ErrorKind)
  where
    I: Into<Idx>,
  {
    self.errors.push(Error {
      idx: idx.into(),
      kind,
    })
  }

  pub(crate) fn mark_errors(&self) -> ErrorsMarker {
    ErrorsMarker {
      bomb: DropBomb::new("must be passed to St::did_error_since"),
      errors_len: self.errors.len(),
    }
  }

  pub(crate) fn did_error_since(&self, mut marker: ErrorsMarker) -> bool {
    marker.bomb.defuse();
    self.errors.len() > marker.errors_len
  }

  pub(crate) fn gen_meta_var(&mut self) -> MetaTyVar {
    self.meta_gen.gen()
  }

  pub(crate) fn gen_from<'a>(
    &'a mut self,
    bound_vars: &'a BoundTyVars,
  ) -> impl Iterator<Item = Ty> + 'a {
    bound_vars.kinds().map(|&x| {
      let mv = self.meta_gen.gen();
      if let Some(k) = x {
        assert!(self.subst.insert(mv.clone(), SubstEntry::Kind(k)).is_none())
      }
      Ty::MetaVar(mv)
    })
  }

  pub(crate) fn gen_fixed_var(&mut self, ty_var: hir::TyVar) -> FixedTyVar {
    self.fixed_gen.gen(ty_var)
  }

  pub(crate) fn finish(self) -> (Syms, Vec<Error>) {
    (self.syms, self.errors)
  }
}

pub(crate) struct ErrorsMarker {
  bomb: DropBomb,
  errors_len: usize,
}
