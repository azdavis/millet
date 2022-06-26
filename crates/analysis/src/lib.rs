//! The unification of all the passes into a single high-level API.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod std_basis;

use fast_hash::FxHashSet;
use paths::{PathId, PathMap};
use std::fmt;
use syntax::ast::{AstNode as _, SyntaxNodePtr};
use syntax::{rowan::TokenAtOffset, SyntaxKind, SyntaxNode};

pub use std_basis::StdBasis;
pub use text_pos::{Position, Range};

/// The name of the root CM file we look for.
pub const ROOT_GROUP: &str = "sources.cm";

/// The max number of errors per path.
pub const MAX_ERRORS_PER_PATH: usize = 20;

/// Performs analysis.
#[derive(Debug, Default)]
pub struct Analysis {
  std_basis: StdBasis,
  files: PathMap<AnalyzedFile>,
  syms: statics::Syms,
}

impl Analysis {
  /// Returns a new `Analysis`.
  pub fn new(std_basis: StdBasis) -> Self {
    Self {
      std_basis,
      files: PathMap::default(),
      syms: statics::Syms::default(),
    }
  }

  /// Given the contents of one isolated file, return the errors for it.
  pub fn get_one(&self, s: &str) -> Vec<Error> {
    let mut file = AnalyzedFile::new(s);
    let mut st = self.std_basis.into_statics();
    let low = &file.lowered;
    let mode = statics::Mode::Regular;
    let (_, es) = statics::get(&mut st, mode, &low.arenas, &low.top_decs);
    file.statics_errors = es;
    file.to_errors(&st.syms)
  }

  /// Given information about many interdependent source files and their groupings, returns a
  /// mapping from source paths to errors.
  pub fn get_many(&mut self, input: &Input) -> PathMap<Vec<Error>> {
    let graph: topo_sort::Graph<_> = input
      .groups
      .iter()
      .map(|(&path, group)| (path, group.dependencies.iter().copied().collect()))
      .collect();
    // TODO error if cycle
    let order = elapsed::log("topo_sort::get", || {
      topo_sort::get(&graph).unwrap_or_default()
    });
    // TODO require explicit basis import
    let mut st = self.std_basis.into_statics();
    self.files = elapsed::log("analyzed_files", || {
      input
        .sources
        .iter()
        .map(|(&path_id, s)| (path_id, AnalyzedFile::new(s)))
        .collect()
    });
    let ret: PathMap<Vec<_>> = elapsed::log("statics", || {
      order
        .into_iter()
        .flat_map(|path| {
          input
            .groups
            .get(&path)
            .into_iter()
            .flat_map(|x| x.source_files.iter())
        })
        .filter_map(|&path_id| {
          let file = match self.files.get_mut(&path_id) {
            Some(x) => x,
            None => {
              log::error!("no file for {path_id:?}");
              return None;
            }
          };
          let low = &file.lowered;
          let mode = statics::Mode::Regular;
          let (info, es) = statics::get(&mut st, mode, &low.arenas, &low.top_decs);
          file.statics_errors = es;
          file.info = info;
          Some((path_id, file.to_errors(&st.syms)))
        })
        .collect()
    });
    self.syms = st.syms;
    ret
  }

  /// Returns a Markdown string with information about this position.
  #[allow(unused_variables)]
  pub fn get_md_info(&self, path: PathId, pos: Position) -> Option<(String, Range)> {
    let file = self.files.get(&path)?;
    let mut node = get_node(file, pos)?;
    loop {
      let ptr = SyntaxNodePtr::new(&node);
      match file.lowered.ptrs.ast_to_hir(ptr.clone()) {
        Some(idx) => {
          let s = file.info.get_md_info(&self.syms, idx)?;
          let range = ptr.to_node(file.parsed.root.syntax()).text_range();
          return Some((s, file.pos_db.range(range)));
        }
        None => node = node.parent()?,
      }
    }
  }

  /// Returns the definition range of the item at this position.
  #[allow(unused_variables)]
  pub fn get_def_location(&self, path: PathId, pos: Position) -> Option<(PathId, Range)> {
    let file = self.files.get(&path)?;
    let mut node = get_node(file, pos)?;
    loop {
      let ptr = SyntaxNodePtr::new(&node);
      match file.lowered.ptrs.ast_to_hir(ptr.clone()) {
        Some(idx) => {
          let def = file.info.get_def_location(idx)?;
          let def_file = self.files.get(&def.path)?;
          let def_range = def_file
            .lowered
            .ptrs
            .hir_to_ast(def.idx)?
            .to_node(def_file.parsed.root.syntax())
            .text_range();
          return Some((def.path, def_file.pos_db.range(def_range)));
        }
        None => node = node.parent()?,
      }
    }
  }
}

fn get_node(file: &AnalyzedFile, pos: Position) -> Option<SyntaxNode> {
  let idx = file.pos_db.text_size(pos);
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
struct AnalyzedFile {
  pos_db: text_pos::PositionDb,
  lex_errors: Vec<lex::Error>,
  parsed: parse::Parse,
  lowered: lower::Lower,
  statics_errors: Vec<statics::Error>,
  info: statics::Info,
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
      pos_db: text_pos::PositionDb::new(s),
      lex_errors: lexed.errors,
      parsed,
      lowered,
      statics_errors: Vec::new(),
      info: statics::Info::default(),
    }
  }

  fn to_errors(&self, syms: &statics::Syms) -> Vec<Error> {
    std::iter::empty()
      .chain(self.lex_errors.iter().map(|err| Error {
        range: self.pos_db.range(err.range()),
        message: err.display().to_string(),
        code: 1000 + u16::from(err.to_code()),
      }))
      .chain(self.parsed.errors.iter().map(|err| Error {
        range: self.pos_db.range(err.range()),
        message: err.display().to_string(),
        code: 2000 + u16::from(err.to_code()),
      }))
      .chain(self.lowered.errors.iter().map(|err| Error {
        range: self.pos_db.range(err.range()),
        message: err.display().to_string(),
        code: 3000 + u16::from(err.to_code()),
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
            .range(syntax.to_node(self.parsed.root.syntax()).text_range()),
          message: err.display(syms).to_string(),
          code: 4000 + u16::from(err.to_code()),
        })
      }))
      .take(MAX_ERRORS_PER_PATH)
      .collect()
  }
}

/// The input to analysis.
#[derive(Debug, Default)]
pub struct Input {
  /// A map from source files to their contents.
  sources: PathMap<String>,
  /// A map from group files to their (parsed) contents.
  groups: PathMap<Group>,
}

impl Input {
  /// Return an iterator over the source files.
  pub fn iter_sources(&self) -> impl Iterator<Item = (paths::PathId, &str)> + '_ {
    self.sources.iter().map(|(&path, s)| (path, s.as_str()))
  }
}

/// An error when getting input.
#[derive(Debug)]
pub struct GetInputError {
  path: std::path::PathBuf,
  kind: GetInputErrorKind,
}

impl GetInputError {
  fn new(path: &std::path::Path, kind: GetInputErrorKind) -> Self {
    Self {
      path: path.to_owned(),
      kind,
    }
  }
}

impl fmt::Display for GetInputError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.path.display(), self.kind)
  }
}

#[derive(Debug)]
enum GetInputErrorKind {
  ReadFile(std::io::Error),
  Cm(cm::Error),
  Canonicalize(std::io::Error),
  NoParent,
  NotInRoot(std::path::StripPrefixError),
}

impl fmt::Display for GetInputErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      GetInputErrorKind::ReadFile(e) => write!(f, "couldn't read file: {e}"),
      GetInputErrorKind::Cm(e) => write!(f, "couldn't process CM file: {e}"),
      GetInputErrorKind::Canonicalize(e) => write!(f, "couldn't canonicalize: {e}"),
      GetInputErrorKind::NoParent => f.write_str("no parent"),
      GetInputErrorKind::NotInRoot(e) => write!(f, "not in root: {e}"),
    }
  }
}

/// Get some input from the filesystem.
pub fn get_input<F>(fs: &F, root: &mut paths::Root) -> Result<Input, GetInputError>
where
  F: paths::FileSystem,
{
  let mut ret = Input::default();
  let root_group_id = get_path_id(fs, root, root.as_path().join(ROOT_GROUP).as_path())?;
  let mut stack = vec![root_group_id];
  while let Some(path_id) = stack.pop() {
    let path = root.get_path(path_id).as_path();
    let s = read_file(fs, path)?;
    let cm = cm::get(&s).map_err(|e| GetInputError::new(path, GetInputErrorKind::Cm(e)))?;
    let parent = match path.parent() {
      Some(x) => x.to_owned(),
      None => return Err(GetInputError::new(path, GetInputErrorKind::NoParent)),
    };
    let mut source_files = Vec::<paths::PathId>::new();
    for path in cm.sml {
      let path = parent.join(path.as_path());
      let path_id = get_path_id(fs, root, path.as_path())?;
      let s = read_file(fs, path.as_path())?;
      source_files.push(path_id);
      ret.sources.insert(path_id, s);
    }
    let mut dependencies = FxHashSet::<paths::PathId>::default();
    for path in cm.cm {
      let path = parent.join(path.as_path());
      let path_id = get_path_id(fs, root, path.as_path())?;
      stack.push(path_id);
      dependencies.insert(path_id);
    }
    let group = Group {
      source_files,
      dependencies,
    };
    ret.groups.insert(path_id, group);
  }
  Ok(ret)
}

/// A group of source files.
///
/// TODO use exports
#[derive(Debug)]
struct Group {
  /// The source file paths, in order.
  source_files: Vec<PathId>,
  /// The dependencies of this group on other groups.
  dependencies: FxHashSet<PathId>,
}

fn get_path_id<F>(
  fs: &F,
  root: &mut paths::Root,
  path: &std::path::Path,
) -> Result<paths::PathId, GetInputError>
where
  F: paths::FileSystem,
{
  let canonical = fs
    .canonicalize(path)
    .map_err(|e| GetInputError::new(path, GetInputErrorKind::Canonicalize(e)))?;
  root
    .get_id(&canonical)
    .map_err(|e| GetInputError::new(path, GetInputErrorKind::NotInRoot(e)))
}

fn read_file<F>(fs: &F, path: &std::path::Path) -> Result<String, GetInputError>
where
  F: paths::FileSystem,
{
  fs.read_to_string(path)
    .map_err(|e| GetInputError::new(path, GetInputErrorKind::ReadFile(e)))
}
