//! Process the syntax of SML/NJ Compilation Manager files.
//!
//! Note that **many** features are not supported.
//!
//! - [The old spec](https://www.smlnj.org/doc/CM/Old/index.html)
//! - [The new spec](https://www.smlnj.org/doc/CM/new.pdf)

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

#[cfg(test)]
mod tests;

mod lex;
mod lower;
mod parse;
mod types;

pub use types::{Class, CmFile, Error, Export, Namespace, PathKind, Result};

/// Turn the contents of a CM file into exports and members.
///
/// # Errors
///
/// If the CM file contents was invalid, or the env didn't define all the path variables.
pub fn get(s: &str, env: &paths::slash_var_path::Env) -> Result<CmFile> {
  let tokens = lex::get(s)?;
  let root = parse::get(&tokens, env)?;
  let file = lower::get(root)?;
  Ok(file)
}
