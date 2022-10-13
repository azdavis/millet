//! The unification of all the passes into a single high-level API.

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

pub mod input;

use diagnostic_util::Error;
use fmt_util::sep_seq;
use paths::{PathId, PathMap, WithPath};
use sml_syntax::ast::{AstNode as _, SyntaxNodePtr};
use sml_syntax::{rowan::TokenAtOffset, SyntaxKind, SyntaxToken};
use std::fmt::{self, Write as _};
use text_pos::{Position, Range};

/// Performs analysis.
#[derive(Debug)]
pub struct Analysis {
  std_basis: mlb_statics::StdBasis,
  error_lines: config::ErrorLines,
  source_files: PathMap<mlb_statics::SourceFile>,
  syms: sml_statics::Syms,
}

impl Analysis {
  /// Returns a new `Analysis`.
  pub fn new(std_basis: mlb_statics::StdBasis, error_lines: config::ErrorLines) -> Self {
    Self {
      std_basis,
      error_lines,
      source_files: PathMap::default(),
      syms: sml_statics::Syms::default(),
    }
  }

  /// Given the contents of one isolated file, return the errors for it.
  pub fn get_one(&self, contents: &str) -> Vec<Error> {
    let mut fix_env = sml_parse::parser::STD_BASIS.clone();
    let (lex_errors, parsed, low) = mlb_statics::start_source_file(contents, &mut fix_env);
    let mut syms = self.std_basis.syms().clone();
    let basis = self.std_basis.basis().clone();
    let mode = sml_statics::Mode::Regular(None);
    let checked = sml_statics::get(&mut syms, &basis, mode, &low.arenas, low.root);
    let mut info = checked.info;
    mlb_statics::add_all_doc_comments(parsed.root.syntax(), &low, &mut info);
    let file = mlb_statics::SourceFile {
      pos_db: text_pos::PositionDb::new(contents),
      lex_errors,
      parsed,
      lowered: low,
      statics_errors: checked.errors,
      info,
    };
    source_file_errors(&file, &syms, self.error_lines)
  }

  /// Given information about many interdependent source files and their groupings, returns a
  /// mapping from source paths to errors.
  pub fn get_many(&mut self, input: &input::Input) -> PathMap<Vec<Error>> {
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
        let err = Error {
          range: group.pos_db.range(err.range())?,
          message: err.to_string(),
          code: err.code(),
          severity: err.severity(),
        };
        Some((path, vec![err]))
      }))
      .chain(self.source_files.iter().map(|(&path, file)| {
        let mut es = source_file_errors(file, &self.syms, self.error_lines);
        for e in es.iter_mut() {
          if let Some(&sev) = input.severities.get(&e.code) {
            e.severity = sev;
          }
        }
        (path, es)
      }))
      .collect()
  }

  /// Returns a Markdown string with information about this position.
  pub fn get_md(&self, pos: WithPath<Position>, token: bool) -> Option<(String, Range)> {
    let ft = self.get_file_and_token(pos)?;
    let mut parts = Vec::<&str>::new();
    let ty_md: Option<String>;
    let range = match ft.get_ptr_and_idx() {
      Some((ptr, idx)) => {
        ty_md = ft.file.info.get_ty_md(&self.syms, idx);
        parts.extend(ty_md.as_deref());
        parts.extend(ft.file.info.get_def(idx).and_then(|def| {
          let info = match def.path {
            sml_statics::DefPath::Regular(path) => &self.source_files.get(&path)?.info,
            sml_statics::DefPath::StdBasis(name) => self.std_basis.get_info(name)?,
          };
          info.get_doc(def.idx)
        }));
        ptr.to_node(ft.file.parsed.root.syntax()).text_range()
      }
      None => ft.token.text_range(),
    };
    if token {
      parts.extend(ft.token.kind().token_doc());
    }
    if parts.is_empty() {
      return None;
    }
    let range = ft.file.pos_db.range(range)?;
    Some((parts.join("\n\n---\n\n"), range))
  }

  /// Returns the range of the definition of the item at this position.
  pub fn get_def(&self, pos: WithPath<Position>) -> Option<WithPath<Range>> {
    let ft = self.get_file_and_token(pos)?;
    let (_, idx) = ft.get_ptr_and_idx()?;
    self.def_to_path_and_range(ft.file.info.get_def(idx)?)
  }

  /// Returns the ranges of the definitions of the types involved in the type of the item at this
  /// position.
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
  pub fn fill_case(&self, pos: WithPath<Position>) -> Option<(Range, String)> {
    let ft = self.get_file_and_token(pos)?;
    let (ptr, _) = ft.get_ptr_and_idx()?;
    let ptr = ptr.cast::<sml_syntax::ast::CaseExp>()?;
    let case = ptr.to_node(ft.file.parsed.root.syntax());
    let range = text_size_util::TextRange::empty(case.syntax().text_range().end());
    let range = ft.file.pos_db.range(range)?;
    let head_ast = case.exp()?;
    let head_ptr = SyntaxNodePtr::new(head_ast.syntax());
    let head = ft.file.lowered.ptrs.ast_to_hir(head_ptr)?;
    let variants = ft.file.info.get_variants(&self.syms, head)?;
    let case = CaseDisplay {
      needs_starting_bar: case.matcher().map_or(false, |x| x.match_rules().count() > 0),
      variants: &variants,
    };
    Some((range, case.to_string()))
  }

  /// Format the given file, and return the end position of the file.
  pub fn format(&self, path: PathId) -> Option<(String, Position)> {
    let file = self.source_files.get(&path)?;
    let mut buf = String::new();
    write!(buf, "{}", sml_fmt::display_root(&file.parsed.root)).ok()?;
    Some((buf, file.pos_db.end_position()))
  }

  fn get_file_and_token(&self, pos: WithPath<Position>) -> Option<FileAndToken<'_>> {
    let file = self.source_files.get(&pos.path)?;
    let idx = file.pos_db.text_size(pos.val)?;
    if !file.parsed.root.syntax().text_range().contains(idx) {
      return None;
    }
    let token = match file.parsed.root.syntax().token_at_offset(idx) {
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
    let path = match def.path {
      sml_statics::DefPath::Regular(p) => p,
      sml_statics::DefPath::StdBasis(_) => return None,
    };
    let def_file = self.source_files.get(&path)?;
    let def_range = def_file
      .lowered
      .ptrs
      .hir_to_ast(def.idx)?
      .to_node(def_file.parsed.root.syntax())
      .text_range();
    Some(path.wrap(def_file.pos_db.range(def_range)?))
  }
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

/// The max number of errors per path.
const MAX_ERRORS_PER_PATH: usize = 20;

fn source_file_errors(
  file: &mlb_statics::SourceFile,
  syms: &sml_statics::Syms,
  lines: config::ErrorLines,
) -> Vec<Error> {
  std::iter::empty()
    .chain(file.lex_errors.iter().filter_map(|err| {
      Some(Error {
        range: file.pos_db.range(err.range())?,
        message: err.display().to_string(),
        code: err.code(),
        severity: err.severity(),
      })
    }))
    .chain(file.parsed.errors.iter().filter_map(|err| {
      Some(Error {
        range: file.pos_db.range(err.range())?,
        message: err.display().to_string(),
        code: err.code(),
        severity: err.severity(),
      })
    }))
    .chain(file.lowered.errors.iter().filter_map(|err| {
      Some(Error {
        range: file.pos_db.range(err.range())?,
        message: err.display().to_string(),
        code: err.code(),
        severity: err.severity(),
      })
    }))
    .chain(file.statics_errors.iter().filter_map(|err| {
      let idx = err.idx();
      let syntax = file.lowered.ptrs.hir_to_ast(idx).expect("no pointer for idx");
      Some(Error {
        range: file.pos_db.range(syntax.to_node(file.parsed.root.syntax()).text_range())?,
        message: err.display(syms, file.info.meta_vars(), lines).to_string(),
        code: err.code(),
        severity: err.severity(),
      })
    }))
    .take(MAX_ERRORS_PER_PATH)
    .collect()
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
      match self.file.lowered.ptrs.ast_to_hir(ptr.clone()) {
        Some(idx) => return Some((ptr, idx)),
        None => node = node.parent()?,
      }
    }
  }
}
