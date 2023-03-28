//! Static analysis.
//!
//! With help from [this article][1].
//!
//! [1]: http://dev.stephendiehl.com/fun/006_hindley_milner.html

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(clippy::too_many_lines, clippy::single_match_else)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

mod compatible;
mod config;
mod dec;
mod display;
mod env;
mod equality;
mod error;
mod exp;
mod fmt_util;
mod generalize;
mod get_env;
mod item;
mod overload;
mod pat;
mod pat_match;
mod st;
mod top_dec;
mod ty;
mod ty_var;
mod types;
mod unify;
mod util;

pub mod basis;
pub mod def;
pub mod disallow;
pub mod info;
pub mod mode;
pub mod path_order;

pub use error::Error;
pub use types::{MetaVarInfo, Syms};

/// The result of statics.
#[derive(Debug)]
pub struct Statics {
  /// The information about the top decs.
  pub info: info::Info,
  /// The errors from the top decs.
  pub errors: Vec<Error>,
  /// The new items defined by the given root.
  pub bs: basis::Bs,
}

/// Does the checks on the root.
pub fn get(
  syms: &mut Syms,
  bs: &basis::Bs,
  mode: mode::Mode,
  arenas: &sml_hir::Arenas,
  root: sml_hir::StrDecIdx,
) -> Statics {
  elapsed::log("sml_statics::get", || {
    let mut st = st::St::new(mode, std::mem::take(syms));
    let bs = top_dec::get(&mut st, bs, arenas, root);
    let (new_syms, errors, mut info) = st.finish();
    info.bs = bs.clone();
    *syms = new_syms;
    Statics { info, errors, bs }
  })
}
