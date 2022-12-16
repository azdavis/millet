//! Static semantics for ML Basis files.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

mod std_basis;

use diagnostic_util::{Code, Severity};
use fast_hash::FxHashMap;
use sml_file_syntax::SourceFileSyntax;
use sml_syntax::ast::AstNode as _;
use std::fmt;

pub use std_basis::StdBasis;

/// The result of analyzing MLB and source files.
#[derive(Debug)]
pub struct MlbStatics {
  /// The errors found in MLB files.
  pub mlb_errors: Vec<Error>,
  /// The generated symbols (types and exceptions and such).
  pub syms: sml_statics::Syms,
  /// A mapping from source file paths to information about them, including errors.
  ///
  /// NOTE see comment in impl about having files analyzed more than once.
  pub source_files: paths::PathMap<SourceFile>,
}

/// A source file.
#[derive(Debug)]
pub struct SourceFile {
  /// The syntax of the source file.
  pub syntax: SourceFileSyntax,
  /// Statics errors from the file.
  pub statics_errors: Vec<sml_statics::Error>,
  /// Statics information from the file.
  pub info: sml_statics::info::Info,
}

/// An error.
#[derive(Debug)]
pub struct Error {
  path: paths::PathId,
  item: Item,
  name: text_size_util::WithRange<str_util::Name>,
}

impl Error {
  /// Returns the path for this.
  #[must_use]
  pub fn path(&self) -> paths::PathId {
    self.path
  }

  /// Returns the range for this.
  #[must_use]
  pub fn range(&self) -> text_size_util::TextRange {
    self.name.range
  }

  /// Returns the code for this.
  #[must_use]
  pub fn code(&self) -> Code {
    Code::n(1017)
  }

  /// Returns the severity for this.
  #[must_use]
  pub fn severity(&self) -> Severity {
    Severity::Error
  }
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "undefined {}: {}", self.item, self.name.val)
  }
}

#[derive(Debug)]
enum Item {
  Basis,
  Structure,
  Signature,
  Functor,
}

impl fmt::Display for Item {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      Item::Basis => "basis",
      Item::Structure => "structure",
      Item::Signature => "signature",
      Item::Functor => "functor",
    };
    f.write_str(s)
  }
}

struct Cx {
  syms: sml_statics::Syms,
  bases: paths::PathMap<MBasis>,
  source_files: paths::PathMap<SourceFile>,
  mlb_errors: Vec<Error>,
}

impl Cx {
  fn undef(
    &mut self,
    path: paths::PathId,
    item: Item,
    name: text_size_util::WithRange<str_util::Name>,
  ) {
    self.mlb_errors.push(Error { path, item, name });
  }
}

#[derive(Debug, Clone, Copy)]
struct Files<'a> {
  source_file_contents: &'a paths::PathMap<String>,
  bas_decs: &'a paths::PathMap<&'a mlb_hir::BasDec>,
  std_basis: &'a MBasis,
}

#[derive(Debug, Default, Clone)]
struct MBasis {
  fix_env: sml_fixity::Env,
  bas_env: FxHashMap<str_util::Name, MBasis>,
  basis: sml_statics::basis::Basis,
}

impl MBasis {
  /// appends other onto self, emptying other.
  fn append(&mut self, other: Self) {
    self.fix_env.extend(other.fix_env);
    self.bas_env.extend(other.bas_env);
    self.basis.append(other.basis);
  }
}

/// Runs analysis.
#[must_use]
pub fn get(
  syms: sml_statics::Syms,
  basis: &sml_statics::basis::Basis,
  source_file_contents: &paths::PathMap<String>,
  bas_decs: &paths::PathMap<&mlb_hir::BasDec>,
  root_group_paths: &[paths::PathId],
) -> MlbStatics {
  let mut cx = Cx {
    syms,
    bases: paths::PathMap::default(),
    source_files: paths::PathMap::default(),
    mlb_errors: Vec::new(),
  };
  for &path in root_group_paths {
    let std_basis = MBasis {
      fix_env: sml_fixity::STD_BASIS.clone(),
      bas_env: FxHashMap::default(),
      basis: basis.clone(),
    };
    let files = Files { source_file_contents, bas_decs, std_basis: &std_basis };
    get_group_file(&mut cx, files, &mut MBasis::default(), path);
  }
  MlbStatics { mlb_errors: cx.mlb_errors, syms: cx.syms, source_files: cx.source_files }
}

fn get_bas_exp(
  cx: &mut Cx,
  files: Files<'_>,
  path: paths::PathId,
  scope: &MBasis,
  ac: &mut MBasis,
  exp: &mlb_hir::BasExp,
) {
  match exp {
    mlb_hir::BasExp::Bas(dec) => get_bas_dec(cx, files, path, scope, ac, dec),
    mlb_hir::BasExp::Name(name) => match scope.bas_env.get(&name.val) {
      None => cx.undef(path, Item::Basis, name.clone()),
      Some(mb) => ac.append(mb.clone()),
    },
    mlb_hir::BasExp::Let(dec, exp) => {
      let mut let_m_basis = MBasis::default();
      get_bas_dec(cx, files, path, scope, &mut let_m_basis, dec);
      let mut scope = scope.clone();
      scope.append(let_m_basis);
      get_bas_exp(cx, files, path, &scope, ac, exp);
    }
  }
}

fn get_bas_dec(
  cx: &mut Cx,
  files: Files<'_>,
  path: paths::PathId,
  scope: &MBasis,
  ac: &mut MBasis,
  dec: &mlb_hir::BasDec,
) {
  match dec {
    mlb_hir::BasDec::Basis(name, exp) => {
      let mut exp_m_basis = MBasis::default();
      get_bas_exp(cx, files, path, scope, &mut exp_m_basis, exp);
      ac.bas_env.insert(name.val.clone(), exp_m_basis);
    }
    mlb_hir::BasDec::Open(name) => match scope.bas_env.get(&name.val) {
      None => cx.undef(path, Item::Basis, name.clone()),
      Some(mb) => ac.append(mb.clone()),
    },
    mlb_hir::BasDec::Local(local_dec, in_dec) => {
      let mut local_m_basis = MBasis::default();
      get_bas_dec(cx, files, path, scope, &mut local_m_basis, local_dec);
      let mut scope = scope.clone();
      scope.append(local_m_basis);
      get_bas_dec(cx, files, path, &scope, ac, in_dec);
    }
    // NOTE this doesn't do any of the stuff with the side conditions with the ty names and whatnot.
    // those might be necessary.
    mlb_hir::BasDec::Export(ns, lhs, rhs) => {
      if !ac.basis.add(*ns, lhs.val.clone(), &scope.basis, &rhs.val) {
        let item = match ns {
          sml_namespace::Namespace::Structure => Item::Structure,
          sml_namespace::Namespace::Signature => Item::Signature,
          sml_namespace::Namespace::Functor => Item::Functor,
        };
        cx.undef(path, item, rhs.clone());
      }
    }
    mlb_hir::BasDec::Seq(decs) => {
      let mut scope = scope.clone();
      for dec in decs {
        let mut one_m_basis = MBasis::default();
        get_bas_dec(cx, files, path, &scope, &mut one_m_basis, dec);
        scope.append(one_m_basis.clone());
        ac.append(one_m_basis);
      }
    }
    mlb_hir::BasDec::Path(path, kind) => match kind {
      mlb_hir::PathKind::Source => {
        let contents = files.source_file_contents.get(path).expect("no source file");
        let mut fix_env = scope.fix_env.clone();
        let syntax = SourceFileSyntax::new(&mut fix_env, contents);
        get_source_file(cx, *path, scope, ac, fix_env, syntax);
      }
      mlb_hir::PathKind::Group => match cx.bases.get(path) {
        Some(mb) => ac.append(mb.clone()),
        None => get_group_file(cx, files, ac, *path),
      },
    },
    mlb_hir::BasDec::SourcePathSet(paths) => {
      let mut syntaxes: paths::PathMap<_> = paths
        .iter()
        .map(|path| {
          let mut fix_env = scope.fix_env.clone();
          let contents = files.source_file_contents.get(path).expect("no source file");
          let syntax = SourceFileSyntax::new(&mut fix_env, contents);
          (*path, (fix_env, syntax))
        })
        .collect();
      let hir_roots: paths::PathMap<_> = syntaxes
        .iter()
        .map(|(&path, (_, syntax))| (path, (&syntax.lower.arenas, syntax.lower.root)))
        .collect();
      assert_eq!(syntaxes.len(), hir_roots.len());
      let order = sml_statics::path_order::get(cx.syms.clone(), scope.basis.clone(), hir_roots);
      assert_eq!(syntaxes.len(), order.len());
      // we could make a sequence of source path defs from the order and recurse on that, but doing
      // it like this lets us avoid re-parsing the syntax. it is a little un-DRY though in the sense
      // of largely duplicating the Seq case.
      let mut scope = scope.clone();
      for path in order {
        let mut one_m_basis = MBasis::default();
        let (fix_env, syntax) = syntaxes.remove(&path).expect("path from order is in syntaxes");
        get_source_file(cx, path, &scope, &mut one_m_basis, fix_env, syntax);
        scope.append(one_m_basis.clone());
        ac.append(one_m_basis);
      }
    }
  }
}

fn get_source_file(
  cx: &mut Cx,
  path: paths::PathId,
  scope: &MBasis,
  ac: &mut MBasis,
  fix_env: sml_fixity::Env,
  syntax: SourceFileSyntax,
) {
  let mode = sml_statics::mode::Mode::Regular(Some(path));
  let checked =
    sml_statics::get(&mut cx.syms, &scope.basis, mode, &syntax.lower.arenas, syntax.lower.root);
  let mut info = checked.info;
  add_all_doc_comments(syntax.parse.root.syntax(), &syntax.lower, &mut info);
  let file = SourceFile { syntax, statics_errors: checked.errors, info };
  ac.append(MBasis { fix_env, bas_env: FxHashMap::default(), basis: checked.basis });
  // NOTE: we would like to assert that the insert returns None, but actually it may not
  // always.
  //
  // this is because a single source file might be included by two different groups. in such a
  // case, it's not actually clear what errors we should emit. (it might be confusing to
  // re-emit some of the same errors, but also maybe have different errors from different
  // analyses, on the same file.)
  //
  // this drops the errors from any previous analyses of this file on the floor.
  cx.source_files.insert(path, file);
}

/// Processes a single group file.
fn get_group_file(cx: &mut Cx, files: Files<'_>, ac: &mut MBasis, path: paths::PathId) {
  let dec = files.bas_decs.get(&path).expect("no bas dec");
  let mut path_ac = MBasis::default();
  get_bas_dec(cx, files, path, files.std_basis, &mut path_ac, dec);
  cx.bases.insert(path, path_ac.clone());
  ac.append(path_ac);
}

/// Adds doc comments in the `root` to the `info`.
pub fn add_all_doc_comments(
  root: &sml_syntax::SyntaxNode,
  low: &sml_lower::Lower,
  info: &mut sml_statics::info::Info,
) {
  let indices = std::iter::empty()
    .chain(low.arenas.pat.iter().map(|(x, _)| sml_hir::Idx::Pat(x)))
    .chain(low.arenas.dec.iter().map(|(x, _)| sml_hir::Idx::Dec(x)))
    .chain(low.arenas.spec.iter().map(|(x, _)| sml_hir::Idx::Spec(x)));
  for idx in indices {
    let ptr = low.ptrs.hir_to_ast(idx).expect("no syntax ptr");
    let node = ptr.to_node(root);
    if let Some(doc) = sml_comment::doc_comment_above(&node) {
      info.add_doc(idx, doc);
    }
  }
}
