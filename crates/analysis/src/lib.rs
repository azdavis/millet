//! The unification of all the passes into a single high-level API.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod std_basis;

use syntax::{ast::AstNode as _, rowan::TextRange};

/// An error.
#[derive(Debug)]
pub struct Error {
  /// The range of the error.
  pub range: TextRange,
  /// The message of the error.
  pub message: String,
}

/// What standard basis to use.
#[derive(Debug, Clone, Copy)]
pub enum StdBasis {
  /// Include most of the standard basis library.
  Full,
  /// Only include fundamental top-level definitions like `int`, `real`, `ref`, `<`, etc.
  Minimal,
}

impl Default for StdBasis {
  fn default() -> Self {
    Self::Full
  }
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
    let mut files: Vec<_> = files.map(AnalyzedFile::new).collect();
    let mut st = match self.std_basis {
      StdBasis::Full => {
        let (syms, bs) = std_basis::SYMS_AND_BS.clone();
        statics::Statics {
          syms,
          bs,
          errors: Vec::new(),
        }
      }
      StdBasis::Minimal => statics::Statics::default(),
    };
    let mode = statics::Mode::Regular;
    for file in files.iter_mut() {
      statics::get(&mut st, mode, &file.lowered.arenas, &file.lowered.top_decs);
      file.statics_errors = std::mem::take(&mut st.errors);
    }
    files
      .into_iter()
      .map(|file| {
        std::iter::empty()
          .chain(file.lex_errors.into_iter().map(|err| Error {
            range: err.range,
            message: err.kind.to_string(),
          }))
          .chain(file.parsed.errors.into_iter().map(|err| Error {
            range: err.range,
            message: err.kind.to_string(),
          }))
          .chain(file.lowered.errors.into_iter().map(|err| Error {
            range: err.range,
            message: err.kind.to_string(),
          }))
          .chain(file.statics_errors.into_iter().filter_map(|err| {
            Some(Error {
              range: file
                .lowered
                .ptrs
                .get(err.idx())?
                .to_node(file.parsed.root.syntax())
                .text_range(),
              message: err.display(&st.syms).to_string(),
            })
          }))
          .collect()
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
}
