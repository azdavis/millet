//! The unification of all the passes into a single high-level API.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

pub mod input;

use diagnostic_util::Diagnostic;
use fmt_util::sep_seq;
use paths::{PathId, PathMap, WithPath};
use sml_syntax::ast::{AstNode as _, SyntaxNodePtr};
use sml_syntax::{rowan::TokenAtOffset, SyntaxKind, SyntaxToken};
use std::fmt;
use text_pos::{Position, Range};

/// Performs analysis.
#[derive(Debug)]
pub struct Analysis {
  std_basis: mlb_statics::StdBasis,
  diagnostics_options: DiagnosticsOptions,
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
  ) -> Self {
    Self {
      std_basis: std_basis.to_mlb_statics(),
      diagnostics_options: DiagnosticsOptions { lines, filter },
      source_files: PathMap::default(),
      syms: sml_statics::Syms::default(),
    }
  }

  /// Given the contents of one isolated file, return the diagnostics for it.
  pub fn get_one(&self, contents: &str) -> Vec<Diagnostic> {
    let mut fix_env = sml_parse::parser::STD_BASIS.clone();
    let syntax = mlb_statics::SourceFileSyntax::new(&mut fix_env, contents);
    let mut syms = self.std_basis.syms().clone();
    let basis = self.std_basis.basis().clone();
    let mode = sml_statics::Mode::Regular(None);
    let checked =
      sml_statics::get(&mut syms, &basis, mode, &syntax.lower.arenas, syntax.lower.root);
    let mut info = checked.info;
    mlb_statics::add_all_doc_comments(syntax.parse.root.syntax(), &syntax.lower, &mut info);
    let file = mlb_statics::SourceFile { syntax, statics_errors: checked.errors, info };
    source_file_diagnostics(&file, &syms, &input::Severities::default(), self.diagnostics_options)
  }

  /// Given information about many interdependent source files and their groupings, returns a
  /// mapping from source paths to diagnostics.
  pub fn get_many(&mut self, input: &input::Input) -> PathMap<Vec<Diagnostic>> {
    let syms = self.std_basis.syms().clone();
    let basis = self.std_basis.basis().clone();
    let groups: paths::PathMap<_> =
      input.groups.iter().map(|(&path, group)| (path, &group.bas_dec)).collect();
    let res = elapsed::log("mlb_statics::get", || {
      mlb_statics::get(syms, basis, &input.sources, &groups, input.root_group_id)
    });
    self.source_files = res.sml;
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
          source_file_diagnostics(file, &self.syms, &input.severities, self.diagnostics_options);
        (path, ds)
      }))
      .collect()
  }

  /// Returns a Markdown string with information about this position.
  #[must_use]
  pub fn get_md(&self, pos: WithPath<Position>, token: bool) -> Option<(String, Range)> {
    let ft = self.get_file_and_token(pos)?;
    let mut parts = Vec::<&str>::new();
    let ty_md: Option<String>;
    let range = match ft.get_ptr_and_idx() {
      Some((ptr, idx)) => {
        ty_md = ft.file.info.get_ty_md(&self.syms, idx);
        parts.extend(ty_md.as_deref());
        parts.extend(ft.file.info.get_def(idx).and_then(|def| match def {
          sml_statics::Def::Path(path, idx) => {
            let info = match path {
              sml_statics::DefPath::Regular(path) => &self.source_files.get(&path)?.info,
              sml_statics::DefPath::BuiltinLib(name) => self.std_basis.get_info(name)?,
            };
            info.get_doc(idx)
          }
          sml_statics::Def::Primitive => None,
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
    let ft = self.get_file_and_token(pos)?;
    let (_, idx) = ft.get_ptr_and_idx()?;
    self.def_to_path_and_range(ft.file.info.get_def(idx)?)
  }

  /// Returns the ranges of the definitions of the types involved in the type of the item at this
  /// position.
  #[must_use]
  pub fn get_ty_defs(&self, pos: WithPath<Position>) -> Option<Vec<WithPath<Range>>> {
    let ft = self.get_file_and_token(pos)?;
    let (_, idx) = ft.get_ptr_and_idx()?;
    Some(
      ft.file
        .info
        .get_ty_defs(&self.syms, idx)?
        .into_iter()
        .filter_map(|def| self.def_to_path_and_range(def))
        .collect(),
    )
  }

  /// Given a position on a `case` expression, return the code and its range to fill the case with
  /// all of the variants of the head's type.
  #[must_use]
  pub fn fill_case(&self, pos: WithPath<Position>) -> Option<(Range, String)> {
    let ft = self.get_file_and_token(pos)?;
    let (ptr, _) = ft.get_ptr_and_idx()?;
    let ptr = ptr.cast::<sml_syntax::ast::CaseExp>()?;
    let case = ptr.to_node(ft.file.syntax.parse.root.syntax());
    let range = text_size_util::TextRange::empty(case.syntax().text_range().end());
    let range = ft.file.syntax.pos_db.range(range)?;
    let head_ast = case.exp()?;
    let head_ptr = SyntaxNodePtr::new(head_ast.syntax());
    let head = ft.file.syntax.lower.ptrs.ast_to_hir(&head_ptr)?;
    let variants = ft.file.info.get_variants(&self.syms, head)?;
    let case = CaseDisplay {
      needs_starting_bar: case.matcher().map_or(false, |x| x.match_rules().count() > 0),
      variants: &variants,
    };
    Some((range, case.to_string()))
  }

  /// Format the given file, and return the end position of the file.
  ///
  /// # Errors
  ///
  /// If there was no file, or if formatting the file failed.
  pub fn format(&self, path: PathId) -> Result<(String, Position), FormatError> {
    let file = self.source_files.get(&path).ok_or(FormatError::NoFile)?;
    let buf = sml_fmt::get(&file.syntax.parse.root).map_err(FormatError::Format)?;
    Ok((buf, file.syntax.pos_db.end_position()))
  }

  fn get_file_and_token(&self, pos: WithPath<Position>) -> Option<FileAndToken<'_>> {
    let file = self.source_files.get(&pos.path)?;
    let idx = file.syntax.pos_db.text_size(pos.val)?;
    if !file.syntax.parse.root.syntax().text_range().contains(idx) {
      return None;
    }
    let token = match file.syntax.parse.root.syntax().token_at_offset(idx) {
      TokenAtOffset::None => return None,
      TokenAtOffset::Single(t) => t,
      TokenAtOffset::Between(t1, t2) => {
        if priority(t1.kind()) >= priority(t2.kind()) {
          t1
        } else {
          t2
        }
      }
    };
    Some(FileAndToken { file, token })
  }

  fn def_to_path_and_range(&self, def: sml_statics::Def) -> Option<WithPath<Range>> {
    let (path, idx) = match def {
      sml_statics::Def::Path(sml_statics::DefPath::Regular(a), b) => (a, b),
      sml_statics::Def::Path(sml_statics::DefPath::BuiltinLib(_), _)
      | sml_statics::Def::Primitive => return None,
    };
    let def_file = self.source_files.get(&path)?;
    let ptr = def_file.syntax.lower.ptrs.hir_to_ast(idx)?;
    let def_range = ptr.to_node(def_file.syntax.parse.root.syntax()).text_range();
    Some(path.wrap(def_file.syntax.pos_db.range(def_range)?))
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
  /// There was no file to format.
  NoFile,
  /// A formatting error.
  Format(sml_fmt::Error),
}

fn priority(kind: SyntaxKind) -> u8 {
  match kind {
    SyntaxKind::Name => 5,
    SyntaxKind::OpKw => 4,
    SyntaxKind::TyVar => 3,
    SyntaxKind::CharLit
    | SyntaxKind::IntLit
    | SyntaxKind::RealLit
    | SyntaxKind::StringLit
    | SyntaxKind::WordLit => 2,
    SyntaxKind::Whitespace | SyntaxKind::BlockComment | SyntaxKind::Invalid => 0,
    _ => 1,
  }
}

#[derive(Debug, Clone, Copy)]
struct DiagnosticsOptions {
  lines: config::ErrorLines,
  filter: config::DiagnosticsFilter,
}

fn diagnostic<M>(
  file: &mlb_statics::SourceFile,
  severities: &input::Severities,
  range: text_size_util::TextRange,
  message: M,
  code: diagnostic_util::Code,
  severity: diagnostic_util::Severity,
) -> Option<Diagnostic>
where
  M: fmt::Display,
{
  let severity = match severities.get(&code) {
    Some(&Some(sev)) => sev,
    Some(None) => return None,
    None => severity,
  };
  Some(Diagnostic {
    range: file.syntax.pos_db.range(range)?,
    message: message.to_string(),
    code,
    severity,
  })
}

/// TODO: we used to limit the max number of diagnostics per file, but now it's trickier because not
/// all diagnostics are "errors", but it would be bad to hit the max number of diagnostics on
/// entirely warnings and then not emit the actual diagnostics. We'd need to come up with a way to
/// order the diagnostics.
fn source_file_diagnostics(
  file: &mlb_statics::SourceFile,
  syms: &sml_statics::Syms,
  severities: &input::Severities,
  options: DiagnosticsOptions,
) -> Vec<Diagnostic> {
  let mut ret: Vec<_> = std::iter::empty()
    .chain(file.syntax.lex_errors.iter().filter_map(|err| {
      diagnostic(file, severities, err.range(), err.display(), err.code(), err.severity())
    }))
    .chain(file.syntax.parse.errors.iter().filter_map(|err| {
      diagnostic(file, severities, err.range(), err.display(), err.code(), err.severity())
    }))
    .chain(file.syntax.lower.errors.iter().filter_map(|err| {
      diagnostic(file, severities, err.range(), err.display(), err.code(), err.severity())
    }))
    .collect();
  if matches!(options.filter, config::DiagnosticsFilter::None) || ret.is_empty() {
    ret.extend(file.statics_errors.iter().filter_map(|err| {
      let idx = err.idx();
      let syntax = file.syntax.lower.ptrs.hir_to_ast(idx).expect("no pointer for idx");
      let node = syntax.to_node(file.syntax.parse.root.syntax());
      let mut range = node.text_range();
      if let Some((case, of)) =
        sml_syntax::ast::CaseExp::cast(node).and_then(|case| case.case_kw().zip(case.of_kw()))
      {
        range = text_size_util::TextRange::new(case.text_range().start(), of.text_range().end());
      }
      let msg = err.display(syms, file.info.meta_vars(), options.lines);
      diagnostic(file, severities, range, msg, err.code(), err.severity())
    }));
  }
  ret
}

struct CaseDisplay<'a> {
  needs_starting_bar: bool,
  variants: &'a [(str_util::Name, bool)],
}

impl fmt::Display for CaseDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "  ")?;
    if self.needs_starting_bar {
      write!(f, "| ")?;
    } else {
      write!(f, "  ")?;
    }
    let iter = self.variants.iter().map(|&(ref name, has_arg)| ArmDisplay { name, has_arg });
    sep_seq(f, "\n  | ", iter)
  }
}

struct ArmDisplay<'a> {
  name: &'a str_util::Name,
  has_arg: bool,
}

impl fmt::Display for ArmDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name)?;
    if self.has_arg {
      write!(f, " _")?;
    }
    write!(f, " => _")
  }
}

struct FileAndToken<'a> {
  file: &'a mlb_statics::SourceFile,
  token: SyntaxToken,
}

impl FileAndToken<'_> {
  fn get_ptr_and_idx(&self) -> Option<(SyntaxNodePtr, sml_hir::Idx)> {
    let mut node = self.token.parent()?;
    loop {
      let ptr = SyntaxNodePtr::new(&node);
      match self.file.syntax.lower.ptrs.ast_to_hir(&ptr) {
        Some(idx) => return Some((ptr, idx)),
        None => node = node.parent()?,
      }
    }
  }
}
