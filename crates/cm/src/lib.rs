//! Handle a CM file.

#![deny(missing_debug_implementations)]
#![deny(rust_2018_idioms)]

#[cfg(test)]
mod tests;

mod lex;
mod lower;
mod parse;
mod types;

pub use types::{Export, Name, Namespace};

/// Turn the contents of a CM file into exports and members.
pub fn get(s: &str) -> anyhow::Result<(Vec<Export>, Vec<std::path::PathBuf>)> {
  let tokens = lex::get(s)?;
  let root = parse::get(&tokens)?;
  let members = lower::get(root)?;
  Ok(members)
}
