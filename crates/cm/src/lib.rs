//! Handle a CM file.

#![deny(rust_2018_idioms)]

mod lex;
mod lower;
mod parse;
mod types;

pub fn get(s: &str) -> anyhow::Result<(Vec<types::Export>, Vec<std::path::PathBuf>)> {
  let tokens = lex::get(s)?;
  let root = parse::get(&tokens)?;
  let members = lower::get(root)?;
  Ok(members)
}
