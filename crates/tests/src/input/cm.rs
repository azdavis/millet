//! Tests for CM syntax.

use cm_syntax::{Export, Namespace, PathKind, PathOrMinus, PathOrStdBasis};
use std::path::PathBuf;
use str_util::Name;

fn check(s: &str, want_exports: Vec<RawExport>, want_paths: &[(&str, PathKind)]) {
  let file = cm_syntax::get(s, &paths::slash_var_path::Env::default()).unwrap();
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
  paths::slash_var_path::get(s, &paths::slash_var_path::Env::default()).unwrap()
}

#[test]
fn group() {
  check(
    r#"
Group is
  ; comment
  hi.sml
  (*
  uh.sml
  *)
  support.sml
"#,
    vec![],
    &[("hi.sml", PathKind::Sml), ("support.sml", PathKind::Sml)],
  );
}

#[test]
fn library() {
  check(
    r#"
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
"#,
    vec![
      mk_name(Namespace::Structure, "A"),
      mk_name(Namespace::Functor, "B"),
      mk_name(Namespace::Signature, "C"),
    ],
    &[
      ("a.sml", PathKind::Sml),
      ("b/c/d.sml", PathKind::Sml),
      ("e.fun", PathKind::Sml),
      ("seq.cm", PathKind::Cm),
      ("f.sig", PathKind::Sml),
      ("uh", PathKind::Sml),
    ],
  );
}

#[test]
fn library_export() {
  check(
    r#"
Library
  structure Foo
  library(quz/baz.cm)
  signature BAR
is
  Foo.sml
  Bar/sources.cm
  quz/baz.cm
"#,
    vec![
      mk_name(Namespace::Structure, "Foo"),
      mk_library("quz/baz.cm"),
      mk_name(Namespace::Signature, "BAR"),
    ],
    &[("Foo.sml", PathKind::Sml), ("Bar/sources.cm", PathKind::Cm), ("quz/baz.cm", PathKind::Cm)],
  );
}

#[test]
fn unknown_class() {
  let e = cm_syntax::get(r#"Group is foo.sml : succ-ml"#, &paths::slash_var_path::Env::default())
    .unwrap_err();
  assert!(e.to_string().contains("unsupported class: succ-ml"));
}
