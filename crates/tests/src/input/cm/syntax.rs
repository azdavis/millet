//! Tests for CM syntax.

use cm_syntax::{Export, Namespace, PathKind, PathOrMinus, PathOrStdBasis};
use std::path::PathBuf;
use str_util::Name;

fn check(s: &str, want_exports: Vec<RawExport>, want_paths: &[(&str, PathKind)]) {
  let file = cm_syntax::get(s, &slash_var_path::Env::default()).unwrap();
  let want_paths: Vec<_> = want_paths.iter().map(|&(s, kind)| (mk_path_buf(s), kind)).collect();
  let got_export = RawExport::from(file.export);
  let got_paths: Vec<_> =
    file.paths.into_iter().map(|path| (path.val.as_path().to_owned(), path.val.kind())).collect();
  assert_eq!(RawExport::Union(want_exports), got_export);
  assert_eq!(want_paths, got_paths);
}

/// Like [`Export`], but without the range info, and thus able to use `==`.
#[derive(Debug, PartialEq, Eq)]
enum RawExport {
  Name(Namespace, Name),
  Library(PathOrStdBasis),
  Source(PathOrMinus),
  Group(PathOrMinus),
  Union(Vec<RawExport>),
  Difference(Box<RawExport>, Box<RawExport>),
  Intersection(Box<RawExport>, Box<RawExport>),
}

impl From<Export> for RawExport {
  fn from(e: Export) -> Self {
    match e {
      Export::Name(ns, n) => RawExport::Name(ns.val, n.val),
      Export::Library(p) => RawExport::Library(p.val),
      Export::Source(p) => RawExport::Source(p.val),
      Export::Group(p) => RawExport::Group(p.val),
      Export::Union(es) => RawExport::Union(es.into_iter().map(RawExport::from).collect()),
      Export::Difference(e1, e2) => {
        RawExport::Difference(Box::new(RawExport::from(*e1)), Box::new(RawExport::from(*e2)))
      }
      Export::Intersection(e1, e2) => {
        RawExport::Intersection(Box::new(RawExport::from(*e1)), Box::new(RawExport::from(*e2)))
      }
    }
  }
}

fn mk_name(ns: Namespace, name: &str) -> RawExport {
  RawExport::Name(ns, Name::new(name))
}

fn mk_library(name: &str) -> RawExport {
  RawExport::Library(PathOrStdBasis::Path(mk_path_buf(name)))
}

fn mk_path_buf(s: &str) -> PathBuf {
  slash_var_path::get(s, &slash_var_path::Env::default()).unwrap()
}

#[test]
fn group() {
  check(
    r"
Group is
  ; comment
  hi.sml
  (*
  uh.sml
  *)
  support.fun
",
    vec![],
    &[
      ("hi.sml", PathKind::Sml(sml_file::Kind::Sml)),
      ("support.fun", PathKind::Sml(sml_file::Kind::Fun)),
    ],
  );
}

#[test]
fn library() {
  check(
    r"
Library
  structure A
  functor B
  signature C
is
  a.sml
  b/c/d.sml
  e.fun
  seq.cm
  f.sig
  uh:sml
",
    vec![
      mk_name(Namespace::Structure, "A"),
      mk_name(Namespace::Functor, "B"),
      mk_name(Namespace::Signature, "C"),
    ],
    &[
      ("a.sml", PathKind::Sml(sml_file::Kind::Sml)),
      ("b/c/d.sml", PathKind::Sml(sml_file::Kind::Sml)),
      ("e.fun", PathKind::Sml(sml_file::Kind::Fun)),
      ("seq.cm", PathKind::Cm),
      ("f.sig", PathKind::Sml(sml_file::Kind::Sig)),
      ("uh", PathKind::Sml(sml_file::Kind::Sml)),
    ],
  );
}

#[test]
fn library_export() {
  check(
    r"
Library
  structure Foo
  library(quz/baz.cm)
  signature BAR
is
  Foo.sml
  Bar/sources.cm
  quz/baz.cm
",
    vec![
      mk_name(Namespace::Structure, "Foo"),
      mk_library("quz/baz.cm"),
      mk_name(Namespace::Signature, "BAR"),
    ],
    &[
      ("Foo.sml", PathKind::Sml(sml_file::Kind::Sml)),
      ("Bar/sources.cm", PathKind::Cm),
      ("quz/baz.cm", PathKind::Cm),
    ],
  );
}

#[test]
fn ml_lex_file() {
  check(
    r"
Library
  functor TigerLexerFun
is
  lexer.lex
",
    vec![mk_name(Namespace::Functor, "TigerLexerFun")],
    &[("lexer.lex", PathKind::MlLex)],
  );
}

#[test]
fn ml_lex_l_file() {
  check(
    r"
Library
  functor TigerLexerFun
is
  lexer.l
",
    vec![mk_name(Namespace::Functor, "TigerLexerFun")],
    &[("lexer.l", PathKind::MlLex)],
  );
}

#[test]
fn ml_yacc_file() {
  check(
    r"
Library
  functor TigerLrValsFun
  signature Tiger_TOKENS
is
  parser.grm
",
    vec![
      mk_name(Namespace::Functor, "TigerLrValsFun"),
      mk_name(Namespace::Signature, "Tiger_TOKENS"),
    ],
    &[("parser.grm", PathKind::MlYacc)],
  );
}

#[test]
fn ml_yacc_y_file() {
  check(
    r"
Library
  functor TigerLrValsFun
  signature Tiger_TOKENS
is
  parser.y
",
    vec![
      mk_name(Namespace::Functor, "TigerLrValsFun"),
      mk_name(Namespace::Signature, "Tiger_TOKENS"),
    ],
    &[("parser.y", PathKind::MlYacc)],
  );
}

#[test]
fn unknown_class() {
  let e =
    cm_syntax::get(r"Group is foo.sml : succ-ml", &slash_var_path::Env::default()).unwrap_err();
  assert!(e.to_string().contains("unsupported class: `succ-ml`"));
}
