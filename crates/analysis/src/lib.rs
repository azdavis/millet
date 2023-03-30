//! The unification of all the passes into a single high-level API.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

mod diagnostic;
mod matcher;
mod source_files;

use paths::{PathId, PathMap, WithPath};
use sml_syntax::ast::{self, AstNode as _, SyntaxNodePtr};
use std::process::{Command, Stdio};
use std::{error::Error, fmt, io::Write as _};
use text_pos::{PositionUtf16, RangeUtf16};
use text_size_util::TextRange;

pub use crate::diagnostic::Diagnostic;
pub use sml_statics::info::CompletionItem;

/// The url to go to for information about diagnostics.
pub const URL: &str = "https://github.com/azdavis/millet/blob/main/docs/diagnostics.md";

/// Performs analysis.
#[derive(Debug)]
pub struct Analysis {
  std_basis: mlb_statics::StdBasis,
  diagnostics_options: diagnostic::Options,
  source_files: PathMap<mlb_statics::SourceFile>,
  syms: sml_statics::Syms,
}

impl Analysis {
  /// Returns a new `Analysis`.
  #[must_use]
  pub fn new(
    std_basis: StdBasis,
    lines: config::ErrorLines,
    ignore: Option<config::init::DiagnosticsIgnore>,
    format: Option<config::init::FormatEngine>,
  ) -> Self {
    Self {
      std_basis: std_basis.to_mlb_statics(),
      diagnostics_options: diagnostic::Options { lines, ignore, format },
      source_files: PathMap::default(),
      syms: sml_statics::Syms::default(),
    }
  }

  /// Given the contents of one isolated file, return the diagnostics for it.
  pub fn get_one(&self, contents: &str) -> Vec<Diagnostic> {
    let mut fix_env = sml_fixity::STD_BASIS.clone();
    let lang = config::lang::Language::default();
    let syntax = sml_file_syntax::SourceFileSyntax::new(&mut fix_env, &lang, contents);
    let mut syms = self.std_basis.syms().clone();
    let basis = self.std_basis.basis().clone();
    let mode = sml_statics::mode::Mode::Regular(None);
    let checked =
      sml_statics::get(&mut syms, &basis, mode, &syntax.lower.arenas, syntax.lower.root);
    let mut info = checked.info;
    mlb_statics::add_all_doc_comments(syntax.parse.root.syntax(), &syntax.lower, &mut info);
    let file = mlb_statics::SourceFile { syntax, statics_errors: checked.errors, info };
    diagnostic::source_file(&file, &syms, self.diagnostics_options)
  }

  /// Given information about many interdependent source files and their groupings, returns a
  /// mapping from source paths to diagnostics.
  pub fn get_many(&mut self, input: &input::Input) -> PathMap<Vec<Diagnostic>> {
    let syms = self.std_basis.syms().clone();
    let mut basis = self.std_basis.basis().clone();
    for path in &input.lang.val {
      // TODO do not ignore failed disallow
      let _ = basis.disallow_val(path);
    }
    let groups: paths::PathMap<_> =
      input.groups.iter().map(|(&path, group)| (path, &group.bas_dec)).collect();
    let res = elapsed::log("mlb_statics::get", || {
      mlb_statics::get(syms, &input.lang, &basis, &input.sources, &groups, &input.root_group_paths)
    });
    self.source_files = res.source_files;
    self.syms = res.syms;
    std::iter::empty()
      .chain(res.mlb_errors.into_iter().filter_map(|err| {
        let path = err.path();
        let group = input.groups.get(&path).expect("no such group");
        let err = Diagnostic {
          range: group.pos_db.range_utf16(err.range())?,
          message: err.to_string(),
          code: err.code(),
          severity: err.severity(),
        };
        Some((path, vec![err]))
      }))
      .chain(self.source_files.iter().map(|(&path, file)| {
        let ds = diagnostic::source_file(file, &self.syms, self.diagnostics_options);
        (path, ds)
      }))
      .map(|(p, ds)| {
        let iter = ds.into_iter().filter_map(|mut d| {
          match input.severities.get(&d.code) {
            Some(&Some(sev)) => d.severity = sev,
            Some(None) => return None,
            None => {}
          }
          Some(d)
        });
        (p, iter.collect())
      })
      .collect()
  }

  /// Returns a Markdown string with information about this position.
  #[must_use]
  pub fn get_md(&self, pos: WithPath<PositionUtf16>, token: bool) -> Option<(String, RangeUtf16)> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let mut parts = Vec::<&str>::new();
    let ty_md: Option<String>;
    let range = match ft.get_ptr_and_idx() {
      Some((ptr, idx)) => {
        ty_md = ft.file.info.get_ty_md(&self.syms, idx);
        parts.extend(ty_md.as_deref());
        parts.extend(ft.file.info.get_defs(idx).filter_map(|def| match def {
          sml_statics::def::Def::Path(path, idx) => {
            let info = match path {
              sml_statics::def::Path::Regular(path) => &self.source_files.get(&path)?.info,
              sml_statics::def::Path::BuiltinLib(name) => self.std_basis.get_info(name)?,
            };
            info.get_doc(idx)
          }
          sml_statics::def::Def::Primitive(prim) => Some(prim.doc()),
        }));
        ptr.text_range()
      }
      None => ft.token.text_range(),
    };
    if token {
      parts.extend(ft.token.kind().token_doc());
    }
    if parts.is_empty() {
      return None;
    }
    let range = ft.file.syntax.pos_db.range_utf16(range)?;
    Some((parts.join("\n\n---\n\n"), range))
  }

  /// Returns the range of the definition of the item at this position.
  #[must_use]
  pub fn get_defs(&self, pos: WithPath<PositionUtf16>) -> Option<Vec<WithPath<RangeUtf16>>> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let (_, idx) = ft.get_ptr_and_idx()?;
    let ret: Vec<_> = ft
      .file
      .info
      .get_defs(idx)
      .filter_map(|def| source_files::path_and_range(&self.source_files, def.to_regular_idx()?))
      .collect();
    Some(ret)
  }

  /// Returns the ranges of the definitions of the types involved in the type of the item at this
  /// position.
  #[must_use]
  pub fn get_ty_defs(&self, pos: WithPath<PositionUtf16>) -> Option<Vec<WithPath<RangeUtf16>>> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let (_, idx) = ft.get_ptr_and_idx()?;
    Some(
      ft.file
        .info
        .get_ty_defs(&self.syms, idx)?
        .into_iter()
        .filter_map(|def| source_files::path_and_range(&self.source_files, def.to_regular_idx()?))
        .collect(),
    )
  }

  /// Given a position on a `case` expression, return the code and its range to fill the case with
  /// all of the variants of the head's type.
  #[must_use]
  pub fn fill_case(&self, pos: WithPath<PositionUtf16>) -> Option<(RangeUtf16, String)> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let (ptr, _) = ft.get_ptr_and_idx()?;
    let ptr = ptr.cast::<ast::CaseExp>()?;
    let case = ptr.try_to_node(ft.file.syntax.parse.root.syntax())?;
    let range = TextRange::empty(case.syntax().text_range().end());
    let range = ft.file.syntax.pos_db.range_utf16(range)?;
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
  /// When formatting the file failed.
  ///
  /// # Panics
  ///
  /// Upon internal error.
  pub fn format(
    &self,
    path: PathId,
    tab_size: u32,
  ) -> Result<(String, PositionUtf16), FormatError> {
    let engine = match self.diagnostics_options.format {
      None => return Err(FormatError::Disabled),
      Some(x) => x,
    };
    let file = self.source_files.get(&path).ok_or(FormatError::NoFile)?;
    let buf = match engine {
      config::init::FormatEngine::Naive => {
        sml_naive_fmt::get(&file.syntax.parse.root, tab_size).map_err(FormatError::NaiveFmt)?
      }
      config::init::FormatEngine::Smlfmt => {
        let contents = file.syntax.parse.root.syntax().to_string();
        let mut prog = Command::new("smlfmt")
          .arg("--stdio")
          .stdin(Stdio::piped())
          .stdout(Stdio::piped())
          .spawn()
          .map_err(SmlfmtError::Spawn)?;
        let mut stdin = prog.stdin.take().unwrap();
        stdin.write_all(contents.as_bytes()).map_err(SmlfmtError::WriteAll)?;
        // explicitly drop to close it
        drop(stdin);
        let output = prog.wait_with_output().map_err(SmlfmtError::Wait)?;
        if !output.status.success() {
          return Err(SmlfmtError::Unsuccessful(output.stderr).into());
        }
        String::from_utf8(output.stdout).map_err(SmlfmtError::Utf8)?
      }
    };
    Ok((buf, file.syntax.pos_db.end_position_utf16()))
  }

  /// Returns the symbols for the file.
  #[must_use]
  pub fn document_symbols(&self, path: PathId) -> Option<Vec<DocumentSymbol>> {
    let file = self.source_files.get(&path)?;
    let ret: Vec<_> = file
      .info
      .document_symbols(&self.syms, path)
      .into_iter()
      .filter_map(|s| symbol(&file.syntax, s))
      .collect();
    Some(ret)
  }

  /// Returns all references to the position.
  #[must_use]
  pub fn find_all_references(
    &self,
    pos: WithPath<PositionUtf16>,
  ) -> Option<Vec<WithPath<RangeUtf16>>> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let (_, indices) = ft.get_ptr_and_indices()?;
    let ret = indices.iter().flat_map(|&idx| {
      let def = sml_statics::def::Def::Path(sml_statics::def::Path::Regular(pos.path), idx);
      self.source_files.iter().flat_map(move |(&path, sf)| {
        sf.info.get_with_def(def).filter_map(move |idx| {
          let ptr = sf.syntax.lower.ptrs.hir_to_ast(idx)?;
          Some(path.wrap(sf.syntax.pos_db.range_utf16(ptr.text_range())?))
        })
      })
    });
    Some(ret.collect())
  }

  /// Returns all completions for the position.
  #[must_use]
  pub fn completions(&self, pos: WithPath<PositionUtf16>) -> Option<Vec<CompletionItem>> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    Some(ft.file.info.completions(&self.syms))
  }

  /// Returns all inlay hints for the range.
  #[must_use]
  pub fn inlay_hints(
    &self,
    range: WithPath<RangeUtf16>,
  ) -> Option<impl Iterator<Item = InlayHint> + '_> {
    let file = self.source_files.get(&range.path)?;
    let ret = file.info.show_ty_annot(&self.syms).filter_map(|(hint, ty_annot)| {
      let idx = sml_hir::Idx::from(hint);
      let ptr = file.syntax.lower.ptrs.hir_to_ast(idx)?;
      // ignore patterns that are not from this exact source
      if file.syntax.lower.ptrs.ast_to_hir_all(&ptr)? != [idx] {
        return None;
      }
      let node = ptr.to_node(file.syntax.parse.root.syntax());
      let parent_kind = node.parent()?.kind();
      // ignore type-annotated patterns
      if sml_syntax::ast::TypedPat::can_cast(parent_kind) {
        return None;
      }
      let range = file.syntax.pos_db.range_utf16(ptr.text_range())?;
      Some([
        InlayHint { position: range.start, label: "(".to_owned(), kind: InlayHintKind::Ty },
        InlayHint { position: range.end, label: ty_annot, kind: InlayHintKind::Ty },
      ])
    });
    Some(ret.flatten())
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
  /// A naive formatting error.
  NaiveFmt(sml_naive_fmt::Error),
  /// A smlfmt error.
  Smlfmt(SmlfmtError),
}

/// An error when running [`smlfmt`][1] as an external process.
///
/// [1]: https://github.com/shwestrick/smlfmt
#[derive(Debug)]
pub enum SmlfmtError {
  /// Couldn't spawn the process.
  Spawn(std::io::Error),
  /// Couldn't write the contents of the file to the process.
  WriteAll(std::io::Error),
  /// Couldn't wait for the process to return the result back.
  Wait(std::io::Error),
  /// The process terminated unsuccessfully, and outputted stderr.
  Unsuccessful(Vec<u8>),
  /// There was a UTF-8 conversion error.
  Utf8(std::string::FromUtf8Error),
}

impl From<SmlfmtError> for FormatError {
  fn from(e: SmlfmtError) -> Self {
    Self::Smlfmt(e)
  }
}

impl fmt::Display for SmlfmtError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      SmlfmtError::Spawn(_) => f.write_str("couldn't spawn `smlfmt`")?,
      SmlfmtError::WriteAll(_) => f.write_str("couldn't write data to `smlfmt`")?,
      SmlfmtError::Wait(_) => f.write_str("couldn't read data from `smlfmt`")?,
      SmlfmtError::Unsuccessful(_) => f.write_str("`smlfmt` exited unsuccessfully")?,
      SmlfmtError::Utf8(_) => f.write_str("couldn't convert `smlfmt` output to UTF-8")?,
    }
    if f.alternate() {
      if let Some(e) = self.source() {
        write!(f, ": {e}")?;
      }
    }
    Ok(())
  }
}

impl Error for SmlfmtError {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    match self {
      SmlfmtError::Spawn(e) | SmlfmtError::WriteAll(e) | SmlfmtError::Wait(e) => Some(e),
      SmlfmtError::Utf8(e) => Some(e),
      SmlfmtError::Unsuccessful(_) => None,
    }
  }
}

/// A symbol.
#[derive(Debug)]
pub struct DocumentSymbol {
  /// The name of the symbol.
  pub name: String,
  /// What kind of symbol this is.
  pub kind: sml_namespace::SymbolKind,
  /// Detail about this symbol.
  pub detail: Option<String>,
  /// The range of the whole symbol.
  pub range: text_pos::RangeUtf16,
  /// The range of just the name.
  pub selection_range: text_pos::RangeUtf16,
  /// Children of this symbol.
  pub children: Vec<DocumentSymbol>,
}

fn symbol(
  file: &sml_file_syntax::SourceFileSyntax,
  sym: sml_statics::info::DocumentSymbol,
) -> Option<DocumentSymbol> {
  let text_range = file.lower.ptrs.hir_to_ast(sym.idx)?.text_range();
  let range = file.pos_db.range_utf16(text_range)?;
  Some(DocumentSymbol {
    name: sym.name,
    kind: sym.kind,
    detail: sym.detail,
    range,
    // TODO improve
    selection_range: range,
    children: sym.children.into_iter().filter_map(|s| symbol(file, s)).collect(),
  })
}

/// An inlay hint.
#[derive(Debug)]
pub struct InlayHint {
  /// The position.
  pub position: text_pos::PositionUtf16,
  /// The label.
  pub label: String,
  /// The kind.
  pub kind: InlayHintKind,
}

/// An inlay hint kind.
#[derive(Debug, Clone, Copy)]
pub enum InlayHintKind {
  /// A parameter.
  Param,
  /// A type.
  Ty,
}
