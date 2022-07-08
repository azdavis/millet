//! Process ML Basis files.
//!
//! From [the MLton docs](http://mlton.org/MLBasis).

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

#[cfg(test)]
mod tests;

mod lex;
mod parse;
mod types;

pub use types::{BasDec, BasExp, Error, ParsedPath, PathKind, Result};

/// Process the contents of a ML Basis file.
pub fn get(s: &str) -> Result<BasDec> {
  let tokens = lex::get(s)?;
  parse::get(&tokens)
}
