//! The unification of all the passes into a single high-level API.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod sml;

pub mod input;

use paths::{PathId, PathMap};
use syntax::ast::{AstNode as _, SyntaxNodePtr};
use syntax::{rowan::TokenAtOffset, SyntaxKind, SyntaxNode};

pub use sml::StdBasis;
pub use text_pos::{Position, Range};

/// The max number of errors per path.
pub const MAX_ERRORS_PER_PATH: usize = 20;

/// Performs analysis.
#[derive(Debug)]
pub struct Analysis {
  std_basis: StdBasis,
  error_lines: config::ErrorLines,
  source_files: PathMap<SourceFile>,
  syms: statics::Syms,
}

impl Analysis {
  /// Returns a new `Analysis`.
  pub fn new(std_basis: StdBasis, error_lines: config::ErrorLines) -> Self {
    Self {
      std_basis,
      error_lines,
      source_files: PathMap::default(),
      syms: statics::Syms::default(),
    }
  }

  /// Given the contents of one isolated file, return the errors for it.
  pub fn get_one(&self, s: &str) -> Vec<Error> {
    let mut file = SourceFile::new(s);
    let mut syms = self.std_basis.syms().clone();
    let mut basis = self.std_basis.basis().clone();
    let low = &file.lowered;
    let mode = statics::Mode::Regular(None);
    let checked = statics::get(&mut syms, &mut basis, mode, &low.arenas, low.root);
    file.statics_errors = checked.errors;
    file.to_errors(&syms, checked.info.meta_vars(), self.error_lines)
  }

  /// Given information about many interdependent source files and their groupings, returns a
  /// mapping from source paths to errors.
  pub fn get_many(&mut self, input: &input::Input) -> PathMap<Vec<Error>> {
    self.source_files = elapsed::log("analyzed_files", || {
      input
        .sources
        .iter()
        .map(|(&path_id, s)| (path_id, SourceFile::new(s)))
        .collect()
    });
    let mut group_bases = PathMap::<statics::basis::Basis>::default();
    let mut source_errors = PathMap::<Vec<Error>>::default();
    self.syms = self.std_basis.syms().clone();
    let res = elapsed::log("statics", || {
      self.group_basis(
        &mut group_bases,
        &mut source_errors,
        &input.groups,
        input.root_group_id,
      )
    });
    match res {
      Ok(()) => {}
      Err(e) => match e {
        GroupBasisError::NoFile(path) => log::error!("no file for {path:?}"),
        GroupBasisError::NoExports(_) => unreachable!(),
      },
    };
    source_errors
  }

  fn group_basis(
    &mut self,
    group_bases: &mut PathMap<statics::basis::Basis>,
    source_errors: &mut PathMap<Vec<Error>>,
    groups: &PathMap<input::Group>,
    path: paths::PathId,
  ) -> Result<(), GroupBasisError> {
    if group_bases.contains_key(&path) {
      return Ok(());
    }
    // TODO require explicit basis import
    let mut basis = self.std_basis.basis().clone();
    let group = match groups.get(&path) {
      Some(x) => x,
      None => return Err(GroupBasisError::NoFile(path)),
    };
    for &path in group.paths.iter() {
      match self.source_files.get_mut(&path) {
        Some(source_file) => {
          let low = &source_file.lowered;
          let mode = statics::Mode::Regular(Some(path));
          let checked = statics::get(&mut self.syms, &mut basis, mode, &low.arenas, low.root);
          // careful with the order here. first assign the statics errors, then get all the
          // errors, then put the info on the source file.
          source_file.statics_errors = checked.errors;
          let errors =
            source_file.to_errors(&self.syms, checked.info.meta_vars(), self.error_lines);
          source_file.info = Some(checked.info);
          source_errors.insert(path, errors);
        }
        None => {
          // if not a source file, must be a group file
          self.group_basis(group_bases, source_errors, groups, path)?;
          let mut other = group_bases
            .get(&path)
            .expect("path should have a basis after successful call")
            .clone();
          basis.append(&mut other);
        }
      }
    }
    let mut exports = group.exports.clone();
    if input::STRICT_EXPORTS {
      basis.limit_with(&mut exports);
      if !exports.is_empty() {
        return Err(GroupBasisError::NoExports(exports));
      }
    }
    group_bases.insert(path, basis);
    Ok(())
  }

  /// Returns a Markdown string with information about this position.
  pub fn get_md(&self, path: PathId, pos: Position) -> Option<(String, Range)> {
    self.go_up_ast(path, pos, |file, ptr, idx| {
      let info = file.info.as_ref()?;
      let mut s = info.get_ty_md(&self.syms, idx)?;
      let def_doc = info.get_def(idx).and_then(|def| {
        let info = match def.path {
          statics::DefPath::Regular(path) => self.source_files.get(&path)?.info.as_ref()?,
          statics::DefPath::StdBasis(name) => self.std_basis.get_info(name)?,
        };
        info.get_doc(def.idx)
      });
      if let Some(def_doc) = def_doc {
        s.push('\n');
        s.push_str(def_doc);
      }
      let range = ptr.to_node(file.parsed.root.syntax()).text_range();
      Some((s, file.pos_db.range(range)?))
    })
  }

  /// Returns the range of the definition of the item at this position.
  pub fn get_def(&self, path: PathId, pos: Position) -> Option<(PathId, Range)> {
    self.go_up_ast(path, pos, |file, _, idx| {
      self.def_to_path_and_range(file.info.as_ref()?.get_def(idx)?)
    })
  }

  /// Returns the ranges of the definitions of the types involved in the type of the item at this
  /// position.
  pub fn get_ty_defs(&self, path: PathId, pos: Position) -> Option<Vec<(PathId, Range)>> {
    self.go_up_ast(path, pos, |file, _, idx| {
      Some(
        file
          .info
          .as_ref()?
          .get_ty_defs(&self.syms, idx)?
          .into_iter()
          .filter_map(|def| self.def_to_path_and_range(def))
          .collect(),
      )
    })
  }

  fn go_up_ast<F, T>(&self, path: PathId, pos: Position, f: F) -> Option<T>
  where
    F: FnOnce(&SourceFile, SyntaxNodePtr, hir::Idx) -> Option<T>,
  {
    let file = self.source_files.get(&path)?;
    let mut node = get_node(file, pos)?;
    loop {
      let ptr = SyntaxNodePtr::new(&node);
      match file.lowered.ptrs.ast_to_hir(ptr.clone()) {
        Some(idx) => return f(file, ptr, idx),
        None => node = node.parent()?,
      }
    }
  }

  fn def_to_path_and_range(&self, def: statics::Def) -> Option<(PathId, Range)> {
    let path = match def.path {
      statics::DefPath::Regular(p) => p,
      statics::DefPath::StdBasis(_) => return None,
    };
    let def_file = self.source_files.get(&path)?;
    let def_range = def_file
      .lowered
      .ptrs
      .hir_to_ast(def.idx)?
      .to_node(def_file.parsed.root.syntax())
      .text_range();
    Some((path, def_file.pos_db.range(def_range)?))
  }
}

fn get_node(file: &SourceFile, pos: Position) -> Option<SyntaxNode> {
  let idx = file.pos_db.text_size(pos)?;
  let tok = match file.parsed.root.syntax().token_at_offset(idx) {
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
  tok.parent()
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

enum GroupBasisError {
  NoFile(paths::PathId),
  NoExports(statics::basis::Exports),
}

/// An error.
#[derive(Debug)]
pub struct Error {
  /// The range of the error.
  pub range: Range,
  /// The message of the error.
  pub message: String,
  /// The error code.
  pub code: u16,
}

#[derive(Debug)]
struct SourceFile {
  pos_db: text_pos::PositionDb,
  lex_errors: Vec<lex::Error>,
  parsed: parse::Parse,
  lowered: lower::Lower,
  statics_errors: Vec<statics::Error>,
  info: Option<statics::Info>,
}

impl SourceFile {
  fn new(s: &str) -> Self {
    let lexed = lex::get(s);
    log::debug!("lex: {:?}", lexed.tokens);
    let parsed = parse::get(&lexed.tokens, &mut parse::parser::STD_BASIS.clone());
    log::debug!("parse: {:#?}", parsed.root);
    let mut lowered = lower::get(&parsed.root);
    ty_var_scope::get(&mut lowered.arenas, lowered.root);
    Self {
      pos_db: text_pos::PositionDb::new(s),
      lex_errors: lexed.errors,
      parsed,
      lowered,
      statics_errors: Vec::new(),
      info: None,
    }
  }

  fn to_errors(
    &self,
    syms: &statics::Syms,
    mv_info: &statics::MetaVarInfo,
    lines: config::ErrorLines,
  ) -> Vec<Error> {
    // TODO: 1000 error codes will be for stuff even before lexing, aka basically the
    // `analysis::input` stage.
    std::iter::empty()
      .chain(self.lex_errors.iter().filter_map(|err| {
        Some(Error {
          range: self.pos_db.range(err.range())?,
          message: err.display().to_string(),
          code: 2000 + u16::from(err.to_code()),
        })
      }))
      .chain(self.parsed.errors.iter().filter_map(|err| {
        Some(Error {
          range: self.pos_db.range(err.range())?,
          message: err.display().to_string(),
          code: 3000 + u16::from(err.to_code()),
        })
      }))
      .chain(self.lowered.errors.iter().filter_map(|err| {
        Some(Error {
          range: self.pos_db.range(err.range())?,
          message: err.display().to_string(),
          code: 4000 + u16::from(err.to_code()),
        })
      }))
      .chain(self.statics_errors.iter().filter_map(|err| {
        let idx = err.idx();
        let syntax = match self.lowered.ptrs.hir_to_ast(idx) {
          Some(x) => x,
          None => {
            log::error!("no pointer for {idx:?}");
            return None;
          }
        };
        Some(Error {
          range: self
            .pos_db
            .range(syntax.to_node(self.parsed.root.syntax()).text_range())?,
          message: err.display(syms, mv_info, lines).to_string(),
          code: 5000 + u16::from(err.to_code()),
        })
      }))
      .take(MAX_ERRORS_PER_PATH)
      .collect()
  }
}
