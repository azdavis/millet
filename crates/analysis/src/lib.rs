//! The unification of all the passes into a single high-level API.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod std_basis;

use syntax::{ast::AstNode as _, rowan::TextRange};

pub use std_basis::StdBasis;

/// An error.
#[derive(Debug)]
pub struct Error {
  /// The range of the error.
  pub range: TextRange,
  /// The message of the error.
  pub message: String,
}

/// Performs analysis.
#[derive(Debug, Default)]
pub struct Analysis {
  std_basis: StdBasis,
}

impl Analysis {
  /// Returns a new `Analysis`.
  pub fn new(std_basis: StdBasis) -> Self {
    Self { std_basis }
  }

  /// Returns a Vec of Vec of errors for each file.
  ///
  /// The length of the returned Vec will be the same as the length of files.
  pub fn get<'a, I>(&self, files: I) -> Vec<Vec<Error>>
  where
    I: Iterator<Item = &'a str>,
  {
    let mut st = self.std_basis.into_statics();
    let mode = statics::Mode::Regular;
    files
      .map(|s| {
        let mut file = AnalyzedFile::new(s);
        statics::get(&mut st, mode, &file.lowered.arenas, &file.lowered.top_decs);
        file.statics_errors = std::mem::take(&mut st.errors);
        file.into_errors(&st.syms).collect()
      })
      .collect()
  }
}

struct AnalyzedFile {
  lex_errors: Vec<lex::Error>,
  parsed: parse::Parse,
  lowered: lower::Lower,
  statics_errors: Vec<statics::Error>,
}

impl AnalyzedFile {
  fn new(s: &str) -> Self {
    let lexed = lex::get(s);
    log::debug!("lex: {:?}", lexed.tokens);
    let parsed = parse::get(&lexed.tokens);
    log::debug!("parse: {:#?}", parsed.root);
    let mut lowered = lower::get(&parsed.root);
    ty_var_scope::get(&mut lowered.arenas, &lowered.top_decs);
    Self {
      lex_errors: lexed.errors,
      parsed,
      lowered,
      statics_errors: Vec::new(),
    }
  }

  fn into_errors(self, syms: &statics::Syms) -> impl Iterator<Item = Error> + '_ {
    std::iter::empty()
      .chain(self.lex_errors.into_iter().map(|err| Error {
        range: err.range,
        message: err.kind.to_string(),
      }))
      .chain(self.parsed.errors.into_iter().map(|err| Error {
        range: err.range,
        message: err.kind.to_string(),
      }))
      .chain(self.lowered.errors.into_iter().map(|err| Error {
        range: err.range,
        message: err.kind.to_string(),
      }))
      .chain(self.statics_errors.into_iter().filter_map(move |err| {
        Some(Error {
          range: self
            .lowered
            .ptrs
            .get(err.idx())?
            .to_node(self.parsed.root.syntax())
            .text_range(),
          message: err.display(syms).to_string(),
        })
      }))
  }
}
