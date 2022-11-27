//! Static analysis.
//!
//! With help from [this article][1].
//!
//! [1]: http://dev.stephendiehl.com/fun/006_hindley_milner.html

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(clippy::too_many_lines, clippy::single_match_else)]

mod compatible;
mod config;
mod dec;
mod equality;
mod error;
mod exp;
mod fmt_util;
mod get_env;
mod info;
mod pat;
mod pat_match;
mod st;
mod top_dec;
mod ty;
mod types;
mod unify;
mod util;

pub mod basis;
pub mod path_order;

pub use error::Error;
pub use info::{Info, Mode};
pub use types::{Def, DefPath, MetaVarInfo, Syms};

/// The result of statics.
#[derive(Debug)]
pub struct Statics {
  /// The information about the top decs.
  pub info: Info,
  /// The errors from the top decs.
  pub errors: Vec<Error>,
  /// The new items defined by the given root.
  pub basis: basis::Basis,
}

/// Does the checks on the root.
pub fn get(
  syms: &mut Syms,
  basis: &basis::Basis,
  mode: Mode,
  arenas: &sml_hir::Arenas,
  root: sml_hir::StrDecIdx,
) -> Statics {
  let mut st = st::St::new(mode, std::mem::take(syms));
  let inner = top_dec::get(&mut st, &basis.inner, arenas, root);
  let (new_syms, errors, info) = st.finish();
  *syms = new_syms;
  Statics { info, errors, basis: basis::Basis { inner } }
}
