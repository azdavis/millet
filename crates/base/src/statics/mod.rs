//! Statics.
//!
//! With help from [this article][1].
//!
//! [1]: http://dev.stephendiehl.com/fun/006_hindley_milner.html

mod ck;
mod std_lib;
mod ty_rzn;
mod types;

use crate::statics::types::{Basis, Result, State};
use ast::TopDec;
use intern::StrRef;
use loc::Located;

/// The data computed when running static analysis.
pub struct Statics {
  bs: Basis,
  st: State,
}

impl Statics {
  #[allow(clippy::new_without_default)]
  /// Returns the initial information to begin running the statics.
  pub fn new() -> Self {
    let (bs, st) = std_lib::get();
    Self { bs, st }
  }

  /// Performs static analysis on a top-level declaration. Returns `Ok(())` iff everything
  /// typechecks.
  pub fn get(&mut self, top_dec: &Located<TopDec<StrRef>>) -> Result<()> {
    ck::ck_top_dec(&mut self.bs, &mut self.st, top_dec)
  }

  /// Finish running the statics.
  pub fn finish(mut self) {
    self.bs.apply(&self.st.subst, &mut self.st.tys);
    assert!(self.bs.free_ty_vars(&self.st.tys).is_empty());
  }
}
