//! The unification of all the passes into a single high-level API.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

mod diagnostic;
mod matcher;
mod source_files;

use paths::{PathId, PathMap, WithPath};
use sml_statics_types::{def, env::Env};
use sml_syntax::ast::{self, AstNode as _, SyntaxNodePtr};
use std::process::{Command, Stdio};
use std::{error::Error, fmt, io::Write as _};
use text_pos::{PositionDb, PositionUtf16, RangeUtf16};
use text_size_util::TextRange;

pub use crate::diagnostic::{Diagnostic, Options};
pub use mlb_statics::StdBasis;

/// The url to go to for information about diagnostics.
pub const URL: &str = "https://github.com/azdavis/millet/blob/main/docs/diagnostics";

/// Performs analysis.
#[derive(Debug)]
pub struct Analysis {
  std_basis: StdBasis,
  diagnostics_options: diagnostic::Options,
  source_files: PathMap<mlb_statics::SourceFile>,
  syms_tys: sml_statics_types::St,
}

impl Analysis {
  /// Returns a new `Analysis`.
  #[must_use]
  pub fn new(std_basis: StdBasis, diagnostics_options: diagnostic::Options) -> Self {
    Self {
      syms_tys: std_basis.syms_tys().clone(),
      std_basis,
      diagnostics_options,
      source_files: PathMap::default(),
    }
  }

  /// Given information about many interdependent source files and their groupings, returns a
  /// mapping from source paths to diagnostics.
  pub fn get_many(
    &mut self,
    input: &input::Input,
  ) -> PathMap<Vec<Diagnostic<text_pos::RangeUtf16>>> {
    self.get_many_impl(input, text_pos::PositionDb::range_utf16)
  }

  /// Given information about many interdependent source files and their groupings, returns a
  /// mapping from source paths to diagnostics.
  pub fn get_many_text_range(
    &mut self,
    input: &input::Input,
  ) -> PathMap<Vec<Diagnostic<TextRange>>> {
    self.get_many_impl(input, |_, b| Some(b))
  }

  fn get_many_impl<F, R>(&mut self, input: &input::Input, f: F) -> PathMap<Vec<Diagnostic<R>>>
  where
    F: Fn(&text_pos::PositionDb, text_size_util::TextRange) -> Option<R>,
  {
    let mut basis = self.std_basis.basis().clone();
    for path in &input.lang.val {
      // TODO do not ignore failed disallow
      _ = basis.disallow_val(path);
    }
    for path in &input.lang.structure {
      // TODO do not ignore failed disallow
      _ = basis.disallow_str(path);
    }
    let groups: paths::PathMap<_> =
      input.groups.iter().map(|(&path, group)| (path, &group.bas_dec)).collect();
    let res = elapsed::log("mlb_statics::get", || {
      mlb_statics::get(
        &mut self.syms_tys,
        &input.lang,
        &basis,
        &input.sources,
        &groups,
        &input.root_group_paths,
      )
    });
    self.source_files = res.source_files;
    std::iter::empty()
      .chain(res.mlb_errors.into_iter().filter_map(|err| {
        let path = err.path();
        let group = input.groups.get(&path).expect("no such group");
        let err = Diagnostic {
          range: f(&group.pos_db, err.range())?,
          message: err.to_string(),
          code: err.code(),
          severity: err.severity(),
        };
        Some((path, vec![err]))
      }))
      .chain(self.source_files.iter().map(|(&path, file)| {
        let ds = diagnostic::source_file(file, &self.syms_tys, self.diagnostics_options, &f);
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

  /// Update only the give path to have the new text, not recalculating diagnostics or anything in
  /// any other paths.
  pub fn update_one(&mut self, input: &input::Input, path: paths::PathId) {
    let source_file = self.source_files.get_mut(&path).expect("no source file");
    let contents = input.sources.get(&path).expect("no contents");
    mlb_statics::update_one(&mut self.syms_tys, &input.lang, source_file, path, contents);
  }

  /// Returns a Markdown string with information about this position.
  #[must_use]
  pub fn get_md(&self, pos: WithPath<PositionUtf16>, token: bool) -> Option<(String, RangeUtf16)> {
    let ft = source_files::file_and_token(&self.source_files, pos)?;
    let mut parts = Vec::<&str>::new();
    let ty_md: Option<String>;
    let range = match ft.get_ptr_and_idx() {
      Some((ptr, idx)) => {
        ty_md = ft.file.info.get_ty_md(&self.syms_tys, idx, self.diagnostics_options.lines);
        parts.extend(ty_md.as_deref());
        let this = def::Def::Path(def::Path::Regular(pos.path), idx);
        parts.extend(self.get_doc(this));
        let defs = ft.file.info.get_defs(idx);
        parts.extend(defs.into_iter().filter_map(|def| self.get_doc(def)));
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

  fn get_doc(&self, def: def::Def) -> Option<&str> {
    match def {
      def::Def::Path(path, idx) => {
        let info = match path {
          def::Path::Regular(path) => &self.source_files.get(&path)?.info,
          def::Path::BuiltinLib(name) => self.std_basis.get_info(name)?,
        };
        info.get_doc(idx)
      }
      def::Def::Primitive(prim) => Some(prim.doc()),
    }
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
      .into_iter()
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
        .get_ty_defs(&self.syms_tys, idx)?
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
    let case = ptr.to_node(ft.file.syntax.parse.root.syntax());
    let range = TextRange::empty(case.syntax().text_range().end());
    let range = ft.file.syntax.pos_db.range_utf16(range)?;
    let head_ast = case.exp()?;
    let head_ptr = SyntaxNodePtr::new(head_ast.syntax());
    let head = ft.file.syntax.lower.ptrs.ast_to_hir(&head_ptr)?;
    let variants = ft.file.info.get_variants(&self.syms_tys, head)?;
    let starting_bar = case.matcher().map_or(false, |x| x.arms().count() > 0);
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
    let file = self.source_files.get(&path).ok_or(FormatError::NoFile)?;
    let buf = match self.diagnostics_options.format {
      config::init::FormatEngine::None => return Err(FormatError::Disabled),
      config::init::FormatEngine::Naive => {
        sml_naive_fmt::get(&file.syntax.parse.root, tab_size).map_err(FormatError::NaiveFmt)?
      }
      config::init::FormatEngine::Smlfmt => {
        let contents = file.syntax.parse.root.syntax().to_string();
        let mut prog = Command::new("smlfmt")
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

  /// Returns the `PositionDb` for the source `path`.
  #[must_use]
  pub fn source_pos_db(&self, path: PathId) -> Option<&PositionDb> {
    Some(&self.source_files.get(&path)?.syntax.pos_db)
  }

  /// Returns the symbols for the file.
  #[must_use]
  pub fn document_symbols(&self, path: PathId) -> Option<Vec<DocumentSymbol>> {
    let file = self.source_files.get(&path)?;
    let ret: Vec<_> = file
      .info
      .document_symbols(&self.syms_tys, path)
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
      let def = def::Def::Path(def::Path::Regular(pos.path), idx);
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
    let mut envs = [&ft.file.scope.env, &ft.file.info.basis().env].map(Some);
    match ft.token.kind() {
      sml_syntax::SyntaxKind::BlockComment
      | sml_syntax::SyntaxKind::Underscore
      | sml_syntax::SyntaxKind::DotDotDot => return None,
      sml_syntax::SyntaxKind::Dot => {
        let grandparent = ft.token.parent()?.parent()?;
        let path = sml_syntax::ast::Path::cast(grandparent)?;
        for env in &mut envs {
          *env = env.and_then(|env| get_env(env, &path));
        }
      }
      _ => {}
    }
    let mut ret = Vec::<CompletionItem>::new();
    for env in envs.into_iter().flatten() {
      self.env_completions(env, &mut ret);
    }
    Some(ret)
  }

  fn env_completions(&self, env: &Env, ac: &mut Vec<CompletionItem>) {
    ac.extend(env.str_env.iter().map(|(name, env)| CompletionItem {
      label: name.as_str().to_owned(),
      kind: sml_namespace::SymbolKind::Structure,
      detail: None,
      documentation: env.def.and_then(|d| self.get_doc(d)).map(ToOwned::to_owned),
    }));
    ac.extend(env.val_env.iter().map(|(name, val_info)| {
      let ty_scheme = val_info.ty_scheme.display(&self.syms_tys, config::DiagnosticLines::Many);
      CompletionItem {
        label: name.as_str().to_owned(),
        kind: sml_symbol_kind::get(&self.syms_tys.tys, val_info),
        detail: Some(ty_scheme.to_string()),
        documentation: val_info.defs.iter().filter_map(|&x| self.get_doc(x)).fold(None, |ac, x| {
          match ac {
            None => Some(x.to_owned()),
            Some(mut ac) => {
              ac.push_str("\n\n---\n\n");
              ac.push_str(x);
              Some(ac)
            }
          }
        }),
      }
    }));
  }

  /// Returns all inlay hints for the range.
  #[must_use]
  pub fn inlay_hints(&self, range: WithPath<RangeUtf16>) -> Option<Vec<InlayHint>> {
    let file = self.source_files.get(&range.path)?;
    let arenas = &file.syntax.lower.arenas;
    let val_bind_pats = arenas
      .dec
      .iter()
      .filter_map(|(_, dec)| match dec {
        sml_hir::Dec::Val(_, val_binds, sml_hir::ValFlavor::Val) => Some(val_binds),
        _ => None,
      })
      .flat_map(|xs| xs.iter().filter_map(|x| x.pat));
    let fun_case_bodies = arenas
      .dec
      .iter()
      .filter_map(|(_, dec)| match dec {
        sml_hir::Dec::Val(_, val_binds, sml_hir::ValFlavor::Fun) => Some(val_binds),
        _ => None,
      })
      .flat_map(|xs| xs.iter().filter_map(|x| x.exp))
      .filter_map(|mut exp| {
        let func = loop {
          match &arenas.exp[exp] {
            sml_hir::Exp::Fn(arms, sml_hir::FnFlavor::FunArg) => exp = arms.first()?.exp?,
            sml_hir::Exp::App(func, _) => break (*func)?,
            _ => unreachable!("non-(FunArg Fn) or App exp for Fun Val"),
          }
        };
        let fst_arm_body = match &arenas.exp[func] {
          sml_hir::Exp::Fn(arms, sml_hir::FnFlavor::FunCase { .. }) => arms.first()?.exp?,
          _ => unreachable!("non-(FunCase Fn) for Fun Val App func"),
        };
        match &arenas.exp[fst_arm_body] {
          sml_hir::Exp::Typed(_, _, sml_hir::TypedFlavor::Fun) => None,
          _ => Some((exp, fst_arm_body)),
        }
      });
    // need to do two iters here because the FunCase tuple case yields many pats,but the Fn and
    // FunCase non-tuple case yield only one.
    let fun_case_tuple_pats = arenas
      .exp
      .iter()
      .filter_map(|(_, exp)| match exp {
        sml_hir::Exp::Fn(cs, sml_hir::FnFlavor::FunCase { tuple: true }) => {
          match &arenas.pat[cs.first()?.pat?] {
            sml_hir::Pat::Record { rows, .. } => Some(rows.iter().filter_map(|&(_, pat)| pat)),
            _ => unreachable!("non-Record pat for FunCase with tuple: true"),
          }
        }
        _ => None,
      })
      .flatten();
    let fn_and_fun_case_non_tuple_pats = arenas.exp.iter().filter_map(|(_, exp)| match exp {
      sml_hir::Exp::Fn(cs, sml_hir::FnFlavor::Fn | sml_hir::FnFlavor::FunCase { tuple: false }) => {
        cs.first()?.pat
      }
      _ => None,
    });
    let ty_hints = val_bind_pats.filter_map(|pat| {
      let (range, ty_annot) = inlay_hint_pat(&self.syms_tys, file, pat)?;
      Some(InlayHint { position: range.end, label: ty_annot, kind: InlayHintKind::Ty })
    });
    let param_hints = std::iter::empty()
      .chain(fun_case_tuple_pats)
      .chain(fn_and_fun_case_non_tuple_pats)
      .filter_map(|pat| {
        let (range, ty_annot) = inlay_hint_pat(&self.syms_tys, file, pat)?;
        Some([
          InlayHint { position: range.start, label: "(".to_owned(), kind: InlayHintKind::Param },
          InlayHint {
            position: range.end,
            label: format!("{ty_annot})"),
            kind: InlayHintKind::Param,
          },
        ])
      })
      .flatten();
    let fun_return_ty_hints = fun_case_bodies.filter_map(|(ptr_exp, exp)| {
      let ptr = file.syntax.lower.ptrs.hir_to_ast(ptr_exp.into())?;
      let fun_bind_ptr = ptr.cast::<sml_syntax::ast::FunBind>()?;
      let fun_bind = fun_bind_ptr.to_node(file.syntax.parse.root.syntax());
      let case = fun_bind.fun_bind_cases().next()?;
      if case.ty_annotation().is_some() {
        return None;
      }
      let end = case.pats().last()?.syntax().text_range().end();
      let position = file.syntax.pos_db.position_utf16(end)?;
      let label = file.info.show_ty_annot(&self.syms_tys, exp)?;
      Some(InlayHint { position, label, kind: InlayHintKind::Ty })
    });
    let hints = std::iter::empty().chain(param_hints).chain(ty_hints).chain(fun_return_ty_hints);
    Some(hints.collect())
  }
}

fn inlay_hint_pat(
  st: &sml_statics_types::St,
  file: &mlb_statics::SourceFile,
  pat: sml_hir::la_arena::Idx<sml_hir::Pat>,
) -> Option<(RangeUtf16, String)> {
  let want = match &file.syntax.lower.arenas.pat[pat] {
    sml_hir::Pat::Typed(_, _) => false,
    sml_hir::Pat::Record { rows, allows_other } => rows.len() > 1 || *allows_other,
    _ => true,
  };
  if !want {
    return None;
  }
  let ty_annot = file.info.show_pat_ty_annot(st, pat)?;
  let ptr = file.syntax.lower.ptrs.hir_to_ast(pat.into())?;
  let range = file.syntax.pos_db.range_utf16(ptr.text_range())?;
  Some((range, ty_annot))
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

/// A completion item.
#[derive(Debug)]
pub struct CompletionItem {
  /// The label.
  pub label: String,
  /// The kind.
  pub kind: sml_namespace::SymbolKind,
  /// Detail about it.
  pub detail: Option<String>,
  /// Markdown documentation for it.
  pub documentation: Option<String>,
}

fn get_env<'e>(mut env: &'e Env, path: &sml_syntax::ast::Path) -> Option<&'e Env> {
  for name in path.name_star_eq_dots() {
    // NOTE: assumes that a NameStarEqDot's first token is the name
    let tok = name.syntax().first_token()?;
    let name = tok.text();
    env = env.str_env.get(name)?;
  }
  Some(env)
}
