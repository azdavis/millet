use crate::types::{Export, Name, Namespace};
use path_slash::PathBufExt as _;
use std::path::PathBuf;

fn check(s: &str, exports: &[(Namespace, &str)], sml: &[&str], cm: &[&str]) {
  let file = crate::get(s).unwrap();
  let want_exports: Vec<_> = exports
    .iter()
    .map(|&(namespace, s)| Export {
      namespace,
      name: Name::new(s),
    })
    .collect();
  let want_sml: Vec<_> = sml.iter().map(|&s| PathBuf::from_slash(s)).collect();
  let want_cm: Vec<_> = cm.iter().map(|&s| PathBuf::from_slash(s)).collect();
  assert_eq!(want_exports, file.exports);
  assert_eq!(want_sml, file.sml);
  assert_eq!(want_cm, file.cm);
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
    &[],
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
    &[
      (Namespace::Structure, "A"),
      (Namespace::Functor, "B"),
      (Namespace::Signature, "C"),
    ],
    &["a.sml", "b/c/d.sml", "e.fun", "f.sig", "uh"],
    &["seq.cm"],
  );
}
