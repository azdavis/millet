//! Handle a CM file.
//!
//! From skimming [the old spec](https://www.smlnj.org/doc/CM/Old/index.html).

#![deny(missing_debug_implementations)]
#![deny(rust_2018_idioms)]

#[cfg(test)]
mod tests;

mod lex;
mod lower;
mod parse;
mod types;

pub use types::{CMFile, Export, Name, Namespace};

/// Turn the contents of a CM file into exports and members.
pub fn get(s: &str) -> anyhow::Result<CMFile> {
  let tokens = lex::get(s)?;
  let root = parse::get(&tokens)?;
  let file = lower::get(root)?;
  Ok(file)
}
