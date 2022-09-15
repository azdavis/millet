use crate::types::{Export, Namespace, PathKind};
use std::path::PathBuf;
use str_util::Name;

fn check(s: &str, want_exports: Vec<RawExport>, want_paths: &[(&str, PathKind)]) {
  let file = crate::get(s, &paths::slash_var_path::Env::default()).unwrap();
  let want_paths: Vec<_> = want_paths.iter().map(|&(s, kind)| (mk_path_buf(s), kind)).collect();
  let got_exports: Vec<_> = file
    .exports
    .into_iter()
    .map(|x| match x {
      Export::Regular(ns, n) => RawExport::Regular(ns.val, n.val),
      Export::Library(p) => RawExport::Library(p.val),
      Export::Source(_) => RawExport::Source,
      Export::Group(_) => RawExport::Group,
    })
    .collect();
  let got_paths: Vec<_> =
    file.paths.into_iter().map(|path| (path.val.path, path.val.kind)).collect();
  assert_eq!(want_exports, got_exports);
  assert_eq!(want_paths, got_paths);
}

#[derive(Debug, PartialEq, Eq)]
enum RawExport {
  Regular(Namespace, Name),
  Library(PathBuf),
  Source,
  Group,
}

fn mk_regular(ns: Namespace, name: &str) -> RawExport {
  RawExport::Regular(ns, Name::new(name))
}

fn mk_library(name: &str) -> RawExport {
  RawExport::Library(mk_path_buf(name))
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
      mk_regular(Namespace::Structure, "A"),
      mk_regular(Namespace::Functor, "B"),
      mk_regular(Namespace::Signature, "C"),
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
      mk_regular(Namespace::Structure, "Foo"),
      mk_library("quz/baz.cm"),
      mk_regular(Namespace::Signature, "BAR"),
    ],
    &[("Foo.sml", PathKind::Sml), ("Bar/sources.cm", PathKind::Cm), ("quz/baz.cm", PathKind::Cm)],
  );
}

#[test]
fn unknown_class() {
  let e = crate::get(r#"Group is foo.sml : succ-ml"#, &paths::slash_var_path::Env::default())
    .unwrap_err();
  assert!(e.to_string().contains("unsupported class: succ-ml"));
}
