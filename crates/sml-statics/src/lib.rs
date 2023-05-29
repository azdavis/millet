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
mod error;
mod exp;
mod get_env;
mod pat;
mod pat_match;
mod st;
mod top_dec;
mod ty;
mod unify;
mod util;

pub mod basis;
pub mod info;
pub mod path_order;

pub use error::Error;

/// The result of statics.
#[derive(Debug)]
pub struct Statics {
  /// The information about the top decs.
  pub info: info::Info,
  /// The errors from the top decs.
  pub errors: Vec<Error>,
  /// Id statuses for path expressions. Only populated when the mode is Dynamics.
  pub exp_id_statuses: sml_statics_types::info::IdStatusMap<sml_hir::Exp>,
  /// Id statuses for path patterns. Only populated when the mode is Dynamics.
  pub pat_id_statuses: sml_statics_types::info::IdStatusMap<sml_hir::Pat>,
}

/// Does the checks on the root.
pub fn get(
  syms_tys: &mut sml_statics_types::St,
  bs: &basis::Bs,
  mode: sml_statics_types::mode::Mode,
  arenas: &sml_hir::Arenas,
  root: &[sml_hir::StrDecIdx],
) -> Statics {
  elapsed::log("sml_statics::get", || {
    let mut st = st::St::new(mode, syms_tys);
    st.info.bs = top_dec::get(&mut st, bs, arenas, root);
    let errors = st.finish();
    Statics {
      info: st.info,
      errors,
      exp_id_statuses: st.exp_id_statuses,
      pat_id_statuses: st.pat_id_statuses,
    }
  })
}
