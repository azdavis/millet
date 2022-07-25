//! The unification of all the passes into a single high-level API.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod error;

pub mod input;

use paths::{PathId, PathMap};
use syntax::ast::{AstNode as _, SyntaxNodePtr};
use syntax::{rowan::TokenAtOffset, SyntaxKind, SyntaxNode};

pub use error::Error;
pub use mlb_statics::StdBasis;
pub use text_pos::{Position, Range};

/// The error number series for "other" errors (i.e. errors NOT from actually analyzing SML files.)
pub const OTHER_ERRORS: u16 = 1000;

/// Performs analysis.
#[derive(Debug)]
pub struct Analysis {
  std_basis: mlb_statics::StdBasis,
  error_lines: config::ErrorLines,
  source_files: PathMap<mlb_statics::SourceFile>,
  syms: statics::Syms,
}

impl Analysis {
  /// Returns a new `Analysis`.
  pub fn new(std_basis: mlb_statics::StdBasis, error_lines: config::ErrorLines) -> Self {
    Self {
      std_basis,
      error_lines,
      source_files: PathMap::default(),
      syms: statics::Syms::default(),
    }
  }

  /// Given the contents of one isolated file, return the errors for it.
  pub fn get_one(&self, contents: &str) -> Vec<Error> {
    let mut fix_env = mlb_statics::STD_BASIS_FIX_ENV.clone();
    let (lex_errors, parsed, low) = mlb_statics::start_source_file(contents, &mut fix_env);
    let mut syms = self.std_basis.syms().clone();
    let basis = self.std_basis.basis().clone();
    let mode = statics::Mode::Regular(None);
    let checked = statics::get(&mut syms, &basis, mode, &low.arenas, low.root);
    let file = mlb_statics::SourceFile {
      pos_db: text_pos::PositionDb::new(contents),
      lex_errors,
      parsed,
      lowered: low,
      statics_errors: checked.errors,
      info: checked.info,
    };
    source_file_errors(&file, &syms, self.error_lines)
  }

  /// Given information about many interdependent source files and their groupings, returns a
  /// mapping from source paths to errors.
  pub fn get_many(&mut self, input: &input::Input) -> PathMap<Vec<Error>> {
    let res = elapsed::log("mlb_statics::get", || {
      mlb_statics::get(
        &self.std_basis,
        &input.sources,
        &input.groups,
        input.root_group_id,
      )
    });
    self.source_files = res.sml;
    self.syms = res.syms;
    std::iter::empty()
      .chain(res.mlb_errors.into_iter().filter_map(|err| {
        let pos_db = match input.groups_pos_dbs.get(&err.path()) {
          Some(x) => x,
          None => {
            log::error!("no group pos db");
            return None;
          }
        };
        Some((
          err.path(),
          vec![Error {
            range: pos_db.range(err.range())?,
            message: err.to_string(),
            code: OTHER_ERRORS + u16::from(err.to_code()),
          }],
        ))
      }))
      .chain(
        self
          .source_files
          .iter()
          .map(|(&path, file)| (path, source_file_errors(file, &self.syms, self.error_lines))),
      )
      .collect()
  }

  /// Returns a Markdown string with information about this position.
  pub fn get_md(&self, path: PathId, pos: Position) -> Option<(String, Range)> {
    self.go_up_ast(path, pos, |file, ptr, idx| {
      let mut s = file.info.get_ty_md(&self.syms, idx)?;
      let def_doc = file.info.get_def(idx).and_then(|def| {
        let info = match def.path {
          statics::DefPath::Regular(path) => &self.source_files.get(&path)?.info,
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
      self.def_to_path_and_range(file.info.get_def(idx)?)
    })
  }

  /// Returns the ranges of the definitions of the types involved in the type of the item at this
  /// position.
  pub fn get_ty_defs(&self, path: PathId, pos: Position) -> Option<Vec<(PathId, Range)>> {
    self.go_up_ast(path, pos, |file, _, idx| {
      Some(
        file
          .info
          .get_ty_defs(&self.syms, idx)?
          .into_iter()
          .filter_map(|def| self.def_to_path_and_range(def))
          .collect(),
      )
    })
  }

  fn go_up_ast<F, T>(&self, path: PathId, pos: Position, f: F) -> Option<T>
  where
    F: FnOnce(&mlb_statics::SourceFile, SyntaxNodePtr, hir::Idx) -> Option<T>,
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

fn get_node(file: &mlb_statics::SourceFile, pos: Position) -> Option<SyntaxNode> {
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

/// The max number of errors per path.
const MAX_ERRORS_PER_PATH: usize = 20;

/// note that this emits no [`OTHER_ERRORS`] errors, since those are for "everything before lexing",
/// aka pretty much all non-SML file errors.
fn source_file_errors(
  file: &mlb_statics::SourceFile,
  syms: &statics::Syms,
  lines: config::ErrorLines,
) -> Vec<Error> {
  std::iter::empty()
    .chain(file.lex_errors.iter().filter_map(|err| {
      Some(Error {
        range: file.pos_db.range(err.range())?,
        message: err.display().to_string(),
        code: 2000 + u16::from(err.to_code()),
      })
    }))
    .chain(file.parsed.errors.iter().filter_map(|err| {
      Some(Error {
        range: file.pos_db.range(err.range())?,
        message: err.display().to_string(),
        code: 3000 + u16::from(err.to_code()),
      })
    }))
    .chain(file.lowered.errors.iter().filter_map(|err| {
      Some(Error {
        range: file.pos_db.range(err.range())?,
        message: err.display().to_string(),
        code: 4000 + u16::from(err.to_code()),
      })
    }))
    .chain(file.statics_errors.iter().filter_map(|err| {
      let idx = err.idx();
      let syntax = match file.lowered.ptrs.hir_to_ast(idx) {
        Some(x) => x,
        None => {
          log::error!("no pointer for {idx:?}");
          return None;
        }
      };
      Some(Error {
        range: file
          .pos_db
          .range(syntax.to_node(file.parsed.root.syntax()).text_range())?,
        message: err.display(syms, file.info.meta_vars(), lines).to_string(),
        code: 5000 + u16::from(err.to_code()),
      })
    }))
    .take(MAX_ERRORS_PER_PATH)
    .collect()
}
