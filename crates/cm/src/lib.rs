//! Process SML/NJ Compilation Manager files.
//!
//! Note that **many** features are not supported.
//!
//! - [The old spec](https://www.smlnj.org/doc/CM/Old/index.html)
//! - [The new spec](https://www.smlnj.org/doc/CM/new.pdf)

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

#[cfg(test)]
mod tests;

mod lex;
mod lower;
mod parse;
mod types;

pub use types::{CMFile, Class, Error, Export, Name, Namespace, Result};

/// Turn the contents of a CM file into exports and members.
pub fn get(s: &str) -> Result<CMFile> {
  let tokens = lex::get(s)?;
  let root = parse::get(&tokens)?;
  let file = lower::get(root)?;
  Ok(file)
}
