//! Static semantics for ML Basis files.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

mod std_basis;

use fast_hash::FxHashMap;
use std::fmt;

pub use std_basis::StdBasis;

/// The result of analyzing MLB and source files.
#[derive(Debug)]
pub struct MlbStatics {
  /// The errors found in MLB files.
  pub errors: Vec<Error>,
  /// The generated symbols (types and exceptions and such).
  pub syms: statics::Syms,
  /// A mapping from source file paths to information about them, including errors.
  ///
  /// NOTE see comment in impl about having files analyzed more than once.
  pub sml: paths::PathMap<SourceFile>,
}

/// A source file.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct SourceFile {
  pub pos_db: text_pos::PositionDb,
  pub lex_errors: Vec<lex::Error>,
  pub parsed: parse::Parse,
  pub lowered: lower::Lower,
  pub statics_errors: Vec<statics::Error>,
  pub info: statics::Info,
}

/// An error.
#[derive(Debug)]
pub struct Error {
  path: paths::PathId,
  item: Item,
  name: located::Located<hir::Name>,
}

impl Error {
  /// Returns the path for this.
  pub fn path(&self) -> paths::PathId {
    self.path
  }

  /// Returns the range for this.
  pub fn range(&self) -> located::TextRange {
    self.name.range
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
  syms: statics::Syms,
  cache: paths::PathMap<MBasis>,
  sml: paths::PathMap<SourceFile>,
  errors: Vec<Error>,
}

impl Cx {
  fn undef(&mut self, path: paths::PathId, item: Item, name: located::Located<hir::Name>) {
    self.errors.push(Error { path, item, name });
  }
}

#[derive(Debug, Clone, Copy)]
struct Files<'a> {
  sml: &'a paths::PathMap<String>,
  mlb: &'a paths::PathMap<mlb_hir::BasDec>,
  std_basis: &'a MBasis,
}

#[derive(Debug, Default, Clone)]
struct MBasis {
  fix_env: parse::parser::FixEnv,
  bas_env: FxHashMap<hir::Name, MBasis>,
  basis: statics::basis::Basis,
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
pub fn get(
  std_basis: &StdBasis,
  sml: &paths::PathMap<String>,
  mlb: &paths::PathMap<mlb_hir::BasDec>,
  root_mlb: paths::PathId,
) -> MlbStatics {
  let mut cx = Cx {
    syms: std_basis.syms().clone(),
    cache: paths::PathMap::default(),
    sml: paths::PathMap::default(),
    errors: Vec::new(),
  };
  let std_basis = MBasis {
    fix_env: parse::parser::STD_BASIS.clone(),
    bas_env: FxHashMap::default(),
    basis: std_basis.basis().clone(),
  };
  let files = Files {
    sml,
    mlb,
    std_basis: &std_basis,
  };
  get_group_file(&mut cx, files, &mut MBasis::default(), root_mlb);
  MlbStatics {
    errors: cx.errors,
    syms: cx.syms,
    sml: cx.sml,
  }
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
    mlb_hir::BasDec::Export(ns, lhs, rhs) => match ns {
      mlb_hir::Namespace::Structure => {
        if !ac.basis.add_str(lhs.val.clone(), &scope.basis, &rhs.val) {
          cx.undef(path, Item::Structure, rhs.clone());
        }
      }
      mlb_hir::Namespace::Signature => {
        if !ac.basis.add_sig(lhs.val.clone(), &scope.basis, &rhs.val) {
          cx.undef(path, Item::Signature, rhs.clone());
        }
      }
      mlb_hir::Namespace::Functor => {
        if !ac.basis.add_fun(lhs.val.clone(), &scope.basis, &rhs.val) {
          cx.undef(path, Item::Functor, rhs.clone());
        }
      }
    },
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
      mlb_hir::PathKind::Sml => {
        let contents = files.sml.get(path).expect("no sml file for path id");
        let mut fix_env = scope.fix_env.clone();
        let (lex_errors, parsed, low) = start_source_file(contents, &mut fix_env);
        let mode = statics::Mode::Regular(Some(*path));
        let checked = statics::get(&mut cx.syms, &scope.basis, mode, &low.arenas, low.root);
        let file = SourceFile {
          pos_db: text_pos::PositionDb::new(contents),
          lex_errors,
          parsed,
          lowered: low,
          statics_errors: checked.errors,
          info: checked.info,
        };
        ac.append(MBasis {
          fix_env,
          bas_env: FxHashMap::default(),
          basis: checked.basis,
        });
        // NOTE: we would like to assert that the insert returns None, but actually it may not
        // always.
        //
        // this is because a single source file might be included by two different groups. in such a
        // case, it's not actually clear what errors we should emit. (it might be confusing to
        // re-emit some of the same errors, but also maybe have different errors from different
        // analyses, on the same file.)
        //
        // this drops the errors from any previous analyses of this file on the floor.
        cx.sml.insert(*path, file);
      }
      mlb_hir::PathKind::Mlb => match cx.cache.get(path) {
        Some(mb) => ac.append(mb.clone()),
        None => get_group_file(cx, files, ac, *path),
      },
    },
  }
}

/// Processes a single source file.
pub(crate) fn start_source_file(
  contents: &str,
  fix_env: &mut parse::parser::FixEnv,
) -> (Vec<lex::Error>, parse::Parse, lower::Lower) {
  let lexed = lex::get(contents);
  let parsed = parse::get(&lexed.tokens, fix_env);
  let mut lowered = lower::get(&parsed.root);
  ty_var_scope::get(&mut lowered.arenas, lowered.root);
  (lexed.errors, parsed, lowered)
}

/// Processes a single group file.
fn get_group_file(cx: &mut Cx, files: Files<'_>, ac: &mut MBasis, path: paths::PathId) {
  let dec = files.mlb.get(&path).expect("no mlb file for path id");
  let mut path_ac = MBasis::default();
  get_bas_dec(cx, files, path, files.std_basis, &mut path_ac, dec);
  cx.cache.insert(path, path_ac.clone());
  ac.append(path_ac);
}
