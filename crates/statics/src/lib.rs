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
mod top_dec;
mod ty;
mod types;
mod unify;
mod util;

pub mod basis;

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
}

/// Does the checks on the root.
pub fn get(
  syms: &mut Syms,
  basis: &mut basis::Basis,
  mode: Mode,
  arenas: &hir::Arenas,
  root: hir::StrDecIdx,
) -> Statics {
  let mut st = st::St::new(mode, std::mem::take(syms));
  let mut bs = types::Bs {
    fun_env: std::mem::take(&mut basis.inner.fun_env),
    sig_env: std::mem::take(&mut basis.inner.sig_env),
    env: types::EnvStack::one(std::mem::take(&mut basis.inner.env)),
  };
  top_dec::get(&mut st, &mut bs, arenas, root);
  let (new_syms, errors, info) = st.finish();
  basis.inner.fun_env = bs.fun_env;
  basis.inner.sig_env = bs.sig_env;
  basis.inner.env = bs.env.into_env();
  *syms = new_syms;
  Statics { info, errors }
}
