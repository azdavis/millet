//! Static analysis.
//!
//! With help from [this article][1].
//!
//! [1]: http://dev.stephendiehl.com/fun/006_hindley_milner.html

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod dec;
mod error;
mod exp;
mod pat;
mod pat_match;
mod st;
mod standard_basis;
mod top_dec;
mod ty;
mod types;
mod unify;
mod util;

pub use error::{Error, Idx};
pub use st::St;
pub use types::Syms;

/// Does the checks.
pub fn get(arenas: &hir::Arenas, top_decs: &[hir::TopDecIdx]) -> (Syms, Vec<Error>) {
  let (syms, mut cx) = standard_basis::get();
  let mut st = st::St::default();
  st.syms = syms;
  for &top_dec in top_decs {
    top_dec::get(&mut st, &mut cx, arenas, top_dec);
  }
  st.finish()
}
