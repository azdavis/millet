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
mod fmt_util;
mod generalizes;
mod get_env;
mod info;
mod pat;
mod pat_match;
mod st;
mod std_basis;
mod top_dec;
mod ty;
mod types;
mod unify;
mod util;

pub use error::Error;
pub use info::Info;
pub use st::{Mode, Statics};
pub use types::{Def, Syms};

/// Does the checks.
pub fn get(
  statics: &mut Statics,
  mode: Mode,
  arenas: &hir::Arenas,
  top_decs: &[hir::StrDecIdx],
) -> (Info, Vec<Error>) {
  let mut st = st::St::new(mode, std::mem::take(&mut statics.syms));
  for &top_dec in top_decs {
    top_dec::get(&mut st, &mut statics.bs, arenas, top_dec);
  }
  let (syms, errors, subst, mut info) = st.finish();
  statics.syms = syms;
  for ty in info.tys_mut() {
    util::apply(&subst, ty);
  }
  (info, errors)
}
