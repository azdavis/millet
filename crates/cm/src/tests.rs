use crate::types::{Export, Name, Namespace};
use path_slash::PathBufExt as _;
use std::path::PathBuf;

fn check(s: &str, want_exports: Vec<Export>, sml: &[&str], cm: &[&str]) {
  let file = crate::get(s).unwrap();
  let want_sml: Vec<_> = sml.iter().map(|&s| PathBuf::from_slash(s)).collect();
  let want_cm: Vec<_> = cm.iter().map(|&s| PathBuf::from_slash(s)).collect();
  assert_eq!(want_exports, file.exports);
  assert_eq!(want_sml, file.sml);
  assert_eq!(want_cm, file.cm);
}

fn mk_regular(ns: Namespace, name: &str) -> Export {
  Export::Regular(ns, Name::new(name))
}

fn mk_library(name: &str) -> Export {
  Export::Library(PathBuf::from_slash(name))
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
    &["hi.sml", "support.sml"],
    &[],
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
      mk_regular(Namespace::Structure, "A"),
      mk_regular(Namespace::Functor, "B"),
      mk_regular(Namespace::Signature, "C"),
    ],
    &["a.sml", "b/c/d.sml", "e.fun", "f.sig", "uh"],
    &["seq.cm"],
  );
}

#[test]
fn dollar_path() {
  check(
    r#"
Group is
  $/basis.cm
  foo.sml
  bar.cm
"#,
    vec![],
    &["foo.sml"],
    &["bar.cm"],
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
      mk_regular(Namespace::Structure, "Foo"),
      mk_library("quz/baz.cm"),
      mk_regular(Namespace::Signature, "BAR"),
    ],
    &["Foo.sml"],
    &["Bar/sources.cm", "quz/baz.cm"],
  );
}
