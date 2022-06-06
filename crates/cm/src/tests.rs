use crate::types::{Export, Name, Namespace};
use path_slash::PathBufExt as _;
use std::path::PathBuf;

fn check(s: &str, exports: &[(Namespace, &str)], members: &[&str]) {
  let (got_exports, got_members) = crate::get(s).unwrap();
  let want_exports: Vec<_> = exports
    .iter()
    .map(|&(namespace, s)| Export {
      namespace,
      name: Name::new(s),
    })
    .collect();
  let want_members: Vec<_> = members.iter().map(|&s| PathBuf::from_slash(s)).collect();
  assert_eq!(want_exports, got_exports);
  assert_eq!(want_members, got_members);
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
  f.sig
  uh:sml
"#,
    &[
      (Namespace::Structure, "A"),
      (Namespace::Functor, "B"),
      (Namespace::Signature, "C"),
    ],
    &["a.sml", "b/c/d.sml", "e.fun", "f.sig", "uh"],
  );
}
