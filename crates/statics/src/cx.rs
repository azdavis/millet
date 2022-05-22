use crate::error::Error;
use crate::types::{MetaTyVar, MetaTyVarGen, Ty};
use rustc_hash::FxHashMap;

#[derive(Debug, Default)]
pub(crate) struct Cx {
  subst: Subst,
  errors: Vec<Error>,
  meta_gen: MetaTyVarGen,
}

impl Cx {
  pub(crate) fn subst(&mut self) -> &mut Subst {
    &mut self.subst
  }

  pub(crate) fn err(&mut self, error: Error) {
    self.errors.push(error);
  }

  pub(crate) fn gen_meta_var(&mut self) -> MetaTyVar {
    self.meta_gen.gen()
  }
}

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
