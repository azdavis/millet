//! The unification of all the passes into a single high-level API.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

mod diagnostics;
mod matcher;
mod source_files;

use diagnostic_util::Diagnostic;
use paths::{PathId, PathMap, WithPath};
use sml_syntax::ast::{self, AstNode as _, SyntaxNodePtr};
use text_pos::{Position, Range};
use text_size_util::TextRange;

/// Performs analysis.
#[derive(Debug)]
pub struct Analysis {
  std_basis: mlb_statics::StdBasis,
  diagnostics_options: diagnostics::Options,
  source_files: PathMap<mlb_statics::SourceFile>,
  syms: sml_statics::Syms,
}

impl Analysis {
  /// Returns a new `Analysis`.
  #[must_use]
  pub fn new(
    std_basis: StdBasis,
    lines: config::ErrorLines,
    filter: config::DiagnosticsFilter,
    format: bool,
  ) -> Self {
    Self {
      std_basis: std_basis.to_mlb_statics(),
      diagnostics_options: diagnostics::Options { lines, filter, format },
      source_files: PathMap::default(),
      syms: sml_statics::Syms::default(),
    }
  }

  /// Given the contents of one isolated file, return the diagnostics for it.
  pub fn get_one(&self, contents: &str) -> Vec<Diagnostic> {
    let mut fix_env = sml_fixity::STD_BASIS.clone();
    let syntax = sml_file_syntax::SourceFileSyntax::new(&mut fix_env, contents);
    let mut syms = self.std_basis.syms().clone();
    let basis = self.std_basis.basis().clone();
    let mode = sml_statics::mode::Mode::Regular(None);
    let checked =
      sml_statics::get(&mut syms, &basis, mode, &syntax.lower.arenas, syntax.lower.root);
    let mut info = checked.info;
    mlb_statics::add_all_doc_comments(syntax.parse.root.syntax(), &syntax.lower, &mut info);
    let file = mlb_statics::SourceFile { syntax, statics_errors: checked.errors, info };
    let severities = input::Severities::default();
    diagnostics::source_file(&file, &syms, &severities, self.diagnostics_options)
  }

  /// Given information about many interdependent source files and their groupings, returns a
  /// mapping from source paths to diagnostics.
  pub fn get_many(&mut self, input: &input::Input) -> PathMap<Vec<Diagnostic>> {
    let syms = self.std_basis.syms().clone();
    let basis = self.std_basis.basis();
    let groups: paths::PathMap<_> =
      input.groups.iter().map(|(&path, group)| (path, &group.bas_dec)).collect();
    let res = elapsed::log("mlb_statics::get", || {
      mlb_statics::get(syms, basis, &input.sources, &groups, &input.root_group_paths)
    });
    self.source_files = res.source_files;
    self.syms = res.syms;
    std::iter::empty()
      .chain(res.mlb_errors.into_iter().filter_map(|err| {
        let path = err.path();
        let group = input.groups.get(&path).expect("no such group");
        let err = Diagnostic {
          range: group.pos_db.range(err.range())?,
          message: err.to_string(),
          code: err.code(),
          severity: err.severity(),
        };
        Some((path, vec![err]))
      }))
      .chain(self.source_files.iter().map(|(&path, file)| {
        let ds =
          diagnostics::source_file(file, &self.syms, &input.severities, self.diagnostics_options);
        (path, ds)
      }))
      .collect()
  }

  /// Returns a Markdown string with information about this position.
  #[must_use]
  pub fn get_md(&self, pos: WithPath<Position>, token: bool) -> Option<(String, Range)> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let mut parts = Vec::<&str>::new();
    let ty_md: Option<String>;
    let range = match ft.get_ptr_and_idx() {
      Some((ptr, idx)) => {
        ty_md = ft.file.info.get_ty_md(&self.syms, idx);
        parts.extend(ty_md.as_deref());
        parts.extend(ft.file.info.get_def(idx).and_then(|def| match def {
          sml_statics::def::Def::Path(path, idx) => {
            let info = match path {
              sml_statics::def::Path::Regular(path) => &self.source_files.get(&path)?.info,
              sml_statics::def::Path::BuiltinLib(name) => self.std_basis.get_info(name)?,
            };
            info.get_doc(idx)
          }
          sml_statics::def::Def::Primitive(prim) => Some(prim.doc()),
        }));
        ptr.to_node(ft.file.syntax.parse.root.syntax()).text_range()
      }
      None => ft.token.text_range(),
    };
    if token {
      parts.extend(ft.token.kind().token_doc());
    }
    if parts.is_empty() {
      return None;
    }
    let range = ft.file.syntax.pos_db.range(range)?;
    Some((parts.join("\n\n---\n\n"), range))
  }

  /// Returns the range of the definition of the item at this position.
  #[must_use]
  pub fn get_def(&self, pos: WithPath<Position>) -> Option<WithPath<Range>> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let (_, idx) = ft.get_ptr_and_idx()?;
    source_files::path_and_range(&self.source_files, ft.file.info.get_def(idx)?)
  }

  /// Returns the ranges of the definitions of the types involved in the type of the item at this
  /// position.
  #[must_use]
  pub fn get_ty_defs(&self, pos: WithPath<Position>) -> Option<Vec<WithPath<Range>>> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let (_, idx) = ft.get_ptr_and_idx()?;
    Some(
      ft.file
        .info
        .get_ty_defs(&self.syms, idx)?
        .into_iter()
        .filter_map(|def| source_files::path_and_range(&self.source_files, def))
        .collect(),
    )
  }

  /// Given a position on a `case` expression, return the code and its range to fill the case with
  /// all of the variants of the head's type.
  #[must_use]
  pub fn fill_case(&self, pos: WithPath<Position>) -> Option<(Range, String)> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let (ptr, _) = ft.get_ptr_and_idx()?;
    let ptr = ptr.cast::<ast::CaseExp>()?;
    let case = ptr.to_node(ft.file.syntax.parse.root.syntax());
    let range = TextRange::empty(case.syntax().text_range().end());
    let range = ft.file.syntax.pos_db.range(range)?;
    let head_ast = case.exp()?;
    let head_ptr = SyntaxNodePtr::new(head_ast.syntax());
    let head = ft.file.syntax.lower.ptrs.ast_to_hir(&head_ptr)?;
    let variants = ft.file.info.get_variants(&self.syms, head)?;
    let starting_bar = case.matcher().map_or(false, |x| x.match_rules().count() > 0);
    let case = matcher::display(starting_bar, &variants);
    Some((range, case.to_string()))
  }

  /// Format the given file, and return the end position of the file.
  ///
  /// # Errors
  ///
  /// - Formatting is disabled
  /// - There was no file to format
  /// - Formatting the file failed
  pub fn format(&self, path: PathId, tab_size: u32) -> Result<(String, Position), FormatError> {
    if !self.diagnostics_options.format {
      return Err(FormatError::Disabled);
    }
    let file = self.source_files.get(&path).ok_or(FormatError::NoFile)?;
    let buf = sml_fmt::get(&file.syntax.parse.root, tab_size).map_err(FormatError::Format)?;
    Ok((buf, file.syntax.pos_db.end_position()))
  }
}

/// A std basis.
#[derive(Debug, Clone, Copy)]
pub enum StdBasis {
  /// The minimal one.
  Minimal,
  /// The full one.
  Full,
}

impl StdBasis {
  fn to_mlb_statics(self) -> mlb_statics::StdBasis {
    match self {
      StdBasis::Minimal => mlb_statics::StdBasis::minimal(),
      StdBasis::Full => mlb_statics::StdBasis::full(),
    }
  }
}

/// An error when formatting a file.
#[derive(Debug)]
pub enum FormatError {
  /// Formatting was disabled.
  Disabled,
  /// There was no file to format.
  NoFile,
  /// A formatting error.
  Format(sml_fmt::Error),
}
