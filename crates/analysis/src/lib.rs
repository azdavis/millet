//! The unification of all the passes into a single high-level API.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod std_basis;

use fast_hash::{FxHashMap, FxHashSet};
use paths::PathId;
use statics::Mode::Regular;
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

/// A group of source files.
///
/// TODO use exports
#[derive(Debug)]
pub struct Group {
  /// The source file paths, in order.
  pub files: Vec<PathId>,
  /// The dependencies of this group on other groups.
  pub dependencies: FxHashSet<PathId>,
}

/// Performs analysis.
#[derive(Debug, Default)]
pub struct Analysis {
  std_basis: StdBasis,
}

/// A map from paths to something.
pub type PathMap<T> = FxHashMap<PathId, T>;

impl Analysis {
  /// Returns a new `Analysis`.
  pub fn new(std_basis: StdBasis) -> Self {
    Self { std_basis }
  }

  /// Given the contents of one isolated file, return the errors for it.
  pub fn get_one(&self, s: &str) -> Vec<Error> {
    let mut f = AnalyzedFile::new(s);
    let mut st = self.std_basis.into_statics();
    statics::get(&mut st, Regular, &f.lowered.arenas, &f.lowered.top_decs);
    f.statics_errors = std::mem::take(&mut st.errors);
    f.into_errors(&st.syms).collect()
  }

  /// Given a mapping from group paths to information about a group, returns a mapping from source
  /// paths to errors.
  ///
  /// TODO remove `get` and rename this to `get`.
  pub fn get_new(
    &self,
    groups: &PathMap<Group>,
    contents: &PathMap<String>,
  ) -> PathMap<Vec<Error>> {
    let graph: topo_sort::Graph<_> = groups
      .iter()
      .map(|(&path, group)| (path, group.dependencies.iter().copied().collect()))
      .collect();
    // TODO error if cycle
    let order = topo_sort::get(&graph).unwrap_or_default();
    // TODO require explicit basis import
    let mut st = self.std_basis.into_statics();
    order
      .into_iter()
      .flat_map(|path| groups.get(&path).into_iter().flat_map(|x| x.files.iter()))
      .filter_map(|path| {
        // TODO panic if fail?
        let s = contents.get(path)?;
        let mut f = AnalyzedFile::new(s);
        statics::get(&mut st, Regular, &f.lowered.arenas, &f.lowered.top_decs);
        f.statics_errors = std::mem::take(&mut st.errors);
        Some((*path, f.into_errors(&st.syms).collect()))
      })
      .collect()
  }

  /// Returns a Vec of Vec of errors for each file.
  ///
  /// The length of the returned Vec will be the same as the length of files.
  pub fn get<'a, I>(&self, files: I) -> Vec<Vec<Error>>
  where
    I: Iterator<Item = &'a str>,
  {
    let mut st = self.std_basis.into_statics();
    files
      .map(|s| {
        let mut f = AnalyzedFile::new(s);
        statics::get(&mut st, Regular, &f.lowered.arenas, &f.lowered.top_decs);
        f.statics_errors = std::mem::take(&mut st.errors);
        f.into_errors(&st.syms).collect()
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
