//! Processing ML Basis files.
//!
//! From [the `MLton` docs](http://mlton.org/MLBasis).

mod lex;
mod parse;
mod types;

pub use types::{BasDec, BasExp, Error, NamesSeq, ParsedPath, PathKind, Result};

/// Process the contents of a ML Basis file.
///
/// # Errors
///
/// If the contents of the file were invalid, or if the env doesn't define all the path vars.
pub fn get(s: &str, env: &slash_var_path::Env) -> Result<BasDec> {
  let tokens = lex::get(s)?;
  parse::get(&tokens, env)
}
