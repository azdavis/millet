use super::{get, Env};
use std::path::PathBuf;

#[track_caller]
fn check(s: &str, env: &[(&str, &str)], want: &[&str]) {
  let env: Env = env.iter().map(|&(k, v)| (k.into(), v.into())).collect();
  let want: PathBuf = want.iter().collect();
  let got = get(s, &env).unwrap();
  assert_eq!(want, got);
}

#[test]
fn empty() {
  check("", &[], &[]);
}

#[test]
fn smoke() {
  check("foo.sml", &[], &["foo.sml"]);
}

#[test]
fn simple() {
  check("foo/bar.sml", &[], &["foo", "bar.sml"]);
}

#[test]
fn var_parens() {
  check("foo/$(BAR)/quz.sml", &[("BAR", "fella")], &["foo", "fella", "quz.sml"]);
}

#[test]
fn var_no_parens() {
  check("foo/$BAR/quz.sml", &[("BAR", "fella")], &["foo", "fella", "quz.sml"]);
}

#[test]
fn var_parens_extra() {
  check("foo/he$(BAR).sml", &[("BAR", "llo")], &["foo", "hello.sml"]);
}

#[test]
fn var_no_parens_extra() {
  check("foo/he$BAR.sml", &[("BAR", "llo")], &["foo", "hello.sml"]);
}

#[test]
fn many_slash() {
  check("foo////bar//quz.mlb", &[], &["foo", "bar", "quz.mlb"]);
}

#[test]
fn empty_var() {
  check("$/basis.cm", &[("", "smlnj")], &["smlnj", "basis.cm"]);
}

#[test]
fn absolute() {
  check("/foo/$BAR/quz.sml", &[("BAR", "hi")], &["/", "foo", "hi", "quz.sml"]);
}

#[test]
fn var_contains_slashes() {
  check("foo/$bar/quz.cm", &[("bar", "a/b/c")], &["foo", "a", "b", "c", "quz.cm"]);
}

#[test]
fn absolute_var_start() {
  check(
    "$SMLNJ-LIB/basis.cm",
    &[("SMLNJ-LIB", "/usr/local/smlnj")],
    &["/", "usr", "local", "smlnj", "basis.cm"],
  );
}
