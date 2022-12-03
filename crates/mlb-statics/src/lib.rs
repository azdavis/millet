//! Static semantics for ML Basis files.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

mod std_basis;

use diagnostic_util::{Code, Severity};
use fast_hash::{FxHashMap, FxHashSet};
use sml_syntax::ast::AstNode;
use std::fmt;
use text_size_util::WithRange;

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
  pub sml: paths::PathMap<SourceFile>,
}

/// A source file.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct SourceFile {
  pub syntax: SourceFileSyntax,
  pub statics_errors: Vec<sml_statics::Error>,
  pub info: sml_statics::Info,
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
  cache: paths::PathMap<MBasis>,
  sml: paths::PathMap<SourceFile>,
  mlb_errors: Vec<Error>,
  /// TODO(equality-checks) remove
  equality_checks: bool,
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
  sml: &'a paths::PathMap<String>,
  mlb: &'a paths::PathMap<&'a BasDec>,
  std_basis: &'a MBasis,
}

#[derive(Debug, Default, Clone)]
struct MBasis {
  fix_env: sml_parse::parser::FixEnv,
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
  sml: &paths::PathMap<String>,
  mlb: &paths::PathMap<&BasDec>,
  root_group_paths: &[paths::PathId],
  equality_checks: bool,
) -> MlbStatics {
  let mut cx = Cx {
    syms,
    cache: paths::PathMap::default(),
    sml: paths::PathMap::default(),
    mlb_errors: Vec::new(),
    equality_checks,
  };
  for &path in root_group_paths {
    let std_basis = MBasis {
      fix_env: sml_parse::parser::STD_BASIS.clone(),
      bas_env: FxHashMap::default(),
      basis: basis.clone(),
    };
    let files = Files { sml, mlb, std_basis: &std_basis };
    get_group_file(&mut cx, files, &mut MBasis::default(), path);
  }
  MlbStatics { mlb_errors: cx.mlb_errors, syms: cx.syms, sml: cx.sml }
}

fn get_bas_exp(
  cx: &mut Cx,
  files: Files<'_>,
  path: paths::PathId,
  scope: &MBasis,
  ac: &mut MBasis,
  exp: &BasExp,
) {
  match exp {
    BasExp::Bas(dec) => get_bas_dec(cx, files, path, scope, ac, dec),
    BasExp::Name(name) => match scope.bas_env.get(&name.val) {
      None => cx.undef(path, Item::Basis, name.clone()),
      Some(mb) => ac.append(mb.clone()),
    },
    BasExp::Let(dec, exp) => {
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
  dec: &BasDec,
) {
  match dec {
    BasDec::Basis(name, exp) => {
      let mut exp_m_basis = MBasis::default();
      get_bas_exp(cx, files, path, scope, &mut exp_m_basis, exp);
      ac.bas_env.insert(name.val.clone(), exp_m_basis);
    }
    BasDec::Open(name) => match scope.bas_env.get(&name.val) {
      None => cx.undef(path, Item::Basis, name.clone()),
      Some(mb) => ac.append(mb.clone()),
    },
    BasDec::Local(local_dec, in_dec) => {
      let mut local_m_basis = MBasis::default();
      get_bas_dec(cx, files, path, scope, &mut local_m_basis, local_dec);
      let mut scope = scope.clone();
      scope.append(local_m_basis);
      get_bas_dec(cx, files, path, &scope, ac, in_dec);
    }
    // NOTE this doesn't do any of the stuff with the side conditions with the ty names and whatnot.
    // those might be necessary.
    BasDec::Export(ns, lhs, rhs) => {
      if !ac.basis.add(*ns, lhs.val.clone(), &scope.basis, &rhs.val) {
        let item = match ns {
          sml_statics::basis::Namespace::Structure => Item::Structure,
          sml_statics::basis::Namespace::Signature => Item::Signature,
          sml_statics::basis::Namespace::Functor => Item::Functor,
        };
        cx.undef(path, item, rhs.clone());
      }
    }
    BasDec::Seq(decs) => {
      let mut scope = scope.clone();
      for dec in decs {
        let mut one_m_basis = MBasis::default();
        get_bas_dec(cx, files, path, &scope, &mut one_m_basis, dec);
        scope.append(one_m_basis.clone());
        ac.append(one_m_basis);
      }
    }
    BasDec::Path(path, kind) => match kind {
      PathKind::Source => {
        let contents = files.sml.get(path).expect("no sml file for path id");
        let mut fix_env = scope.fix_env.clone();
        let syntax = SourceFileSyntax::new(&mut fix_env, contents);
        get_source_file(cx, *path, scope, ac, fix_env, syntax);
      }
      PathKind::Group => match cx.cache.get(path) {
        Some(mb) => ac.append(mb.clone()),
        None => get_group_file(cx, files, ac, *path),
      },
    },
    BasDec::SourcePathSet(paths) => {
      let mut syntaxes: paths::PathMap<_> = paths
        .iter()
        .map(|path| {
          let mut fix_env = scope.fix_env.clone();
          let contents = files.sml.get(path).expect("no sml file for path id");
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
  fix_env: sml_parse::parser::FixEnv,
  syntax: SourceFileSyntax,
) {
  let mode = sml_statics::Mode::Regular(Some(path), cx.equality_checks);
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
  cx.sml.insert(path, file);
}

/// A source file analyzed at the purely syntactic level.
#[derive(Debug)]
pub struct SourceFileSyntax {
  /// The position database for this file.
  pub pos_db: text_pos::PositionDb,
  /// Lex errors from the file.
  pub lex_errors: Vec<sml_lex::Error>,
  /// The lossless concrete syntax tree.
  pub parse: sml_parse::Parse,
  /// The lowered HIR.
  pub lower: sml_lower::Lower,
}

impl SourceFileSyntax {
  /// Starts processing a single source file.
  pub fn new(fix_env: &mut sml_parse::parser::FixEnv, contents: &str) -> Self {
    let (lex_errors, parse) = Self::lex_and_parse(fix_env, contents);
    let mut lower = sml_lower::get(&parse.root);
    sml_ty_var_scope::get(&mut lower.arenas, lower.root);
    Self { pos_db: text_pos::PositionDb::new(contents), lex_errors, parse, lower }
  }

  /// Lex and parse a source file.
  pub fn lex_and_parse(
    fix_env: &mut sml_parse::parser::FixEnv,
    contents: &str,
  ) -> (Vec<sml_lex::Error>, sml_parse::Parse) {
    let lexed = sml_lex::get(contents);
    let parse = sml_parse::get(&lexed.tokens, fix_env);
    (lexed.errors, parse)
  }
}

/// Processes a single group file.
fn get_group_file(cx: &mut Cx, files: Files<'_>, ac: &mut MBasis, path: paths::PathId) {
  let dec = files.mlb.get(&path).expect("no mlb file for path id");
  let mut path_ac = MBasis::default();
  get_bas_dec(cx, files, path, files.std_basis, &mut path_ac, dec);
  cx.cache.insert(path, path_ac.clone());
  ac.append(path_ac);
}

/// Adds doc comments in the `root` to the `info`.
pub fn add_all_doc_comments(
  root: &sml_syntax::SyntaxNode,
  low: &sml_lower::Lower,
  info: &mut sml_statics::Info,
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

/// A basis declaration.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum BasDec {
  Basis(WithRange<str_util::Name>, Box<BasExp>),
  Open(WithRange<str_util::Name>),
  Local(Box<BasDec>, Box<BasDec>),
  Export(sml_statics::basis::Namespace, WithRange<str_util::Name>, WithRange<str_util::Name>),
  Seq(Vec<BasDec>),
  Path(paths::PathId, PathKind),
  /// Used by CM.
  SourcePathSet(FxHashSet<paths::PathId>),
}

impl BasDec {
  /// Returns a sequence of decs.
  ///
  /// # Panics
  ///
  /// If there was an internal error.
  #[must_use]
  pub fn seq(mut decs: Vec<Self>) -> Self {
    if decs.len() == 1 {
      decs.pop().unwrap()
    } else {
      Self::Seq(decs)
    }
  }
}

/// A basis expression.
#[derive(Debug)]
#[allow(missing_docs)]
pub enum BasExp {
  Bas(BasDec),
  Name(WithRange<str_util::Name>),
  Let(BasDec, Box<BasExp>),
}

/// A kind of path.
#[derive(Debug, Clone, Copy)]
pub enum PathKind {
  /// An SML source path.
  Source,
  /// A group path, like MLB or CM.
  Group,
}
