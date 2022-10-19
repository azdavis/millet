//! Low-level tests for analysis input.

use crate::check::ROOT;

#[test]
fn arbitrary_root_group() {
  check_empty_cm(&["foo.cm"], None).unwrap();
}

#[test]
fn no_root_group_empty() {
  let e = check_empty_cm(&[], None).unwrap_err();
  assert!(e.to_string().contains("no root group file"));
}

#[test]
fn no_root_group_wrong_ext() {
  let e = check_input([("foo.txt", "hi there"), ("foo.rs", "fn main() {}")], None).unwrap_err();
  assert!(e.to_string().contains("no root group file"));
}

#[test]
fn multiple_root_groups_err() {
  let e = check_empty_cm(&["foo.cm", "bar.cm"], None).unwrap_err();
  assert!(e.to_string().contains("multiple root group files"));
}

#[test]
fn multiple_root_groups_ok() {
  let config = r#"
version = 1
[workspace]
# prefer foo over bar
root = "foo.cm"
"#;
  check_empty_cm(&["foo.cm", "bar.cm"], Some(config)).unwrap();
}

#[test]
fn no_root_group_in_config_ok() {
  check_empty_cm(&["sources.cm"], Some("version = 1")).unwrap();
}

#[test]
fn config_invalid_version() {
  let e = check_empty_cm(&["sources.cm"], Some("version = 123")).unwrap_err();
  assert!(e.to_string().contains("invalid config version"));
}

#[test]
fn config_path_not_exist() {
  let config = r#"
version = 1
workspace.root = "nope.cm"
"#;
  check_empty_cm(&["foo.cm"], Some(config)).unwrap_err();
}

#[test]
fn config_parse_err() {
  let e = check_empty_cm(&["foo.cm"], Some("岡部倫太郎")).unwrap_err();
  assert!(e.to_string().contains("couldn't parse config"));
}

#[test]
fn cycle_1() {
  let e = check_input([("foo.cm", "Group is foo.cm")], None).unwrap_err();
  assert!(e.to_string().contains("there is a cycle"));
}

#[test]
fn cycle_2() {
  let inp = [("foo.cm", "Group is bar.cm"), ("bar.cm", "Group is foo.cm")];
  let config = r#"
version = 1
[workspace]
root = "foo.cm"
  "#;
  let e = check_input(inp, Some(config)).unwrap_err();
  assert!(e.to_string().contains("there is a cycle"));
}

#[test]
fn not_group() {
  let config = r#"
version = 1
workspace.root = "nope.txt"
"#;
  let e = check_empty_cm(&["foo.cm"], Some(config)).unwrap_err();
  assert!(e.to_string().contains("not a group file path"));
}

#[test]
fn unknown_property() {
  let config = r#"
version = 1
foo = "bar"
workspace.woofer = "bark.txt"
[quz]
chihiro = true
"#;
  check_empty_cm(&["foo.cm"], Some(config)).unwrap();
}

#[test]
fn members_and_root() {
  let config = r#"
version = 1
[workspace]
members = ["*"]
root = "foo.cm"
"#;
  let e = check_empty_cm(&["foo.cm"], Some(config)).unwrap_err();
  assert!(e.to_string().contains("cannot set `workspace.members` but also set"));
}

#[test]
fn members_and_path_vars() {
  let config = r#"
version = 1
[workspace]
members = ["*"]
path-vars.FOO = { value = "BAR" }
"#;
  let e = check_empty_cm(&["foo.cm"], Some(config)).unwrap_err();
  assert!(e.to_string().contains("cannot set `workspace.members` but also set"));
}

#[test]
fn members() {
  let config = r#"
version = 1
[workspace]
members = ["*"]
"#;
  check_empty_cm(&["foo.cm"], Some(config)).unwrap();
}

#[test]
fn mlb() {
  check_input([("foo.mlb", "")], None).unwrap();
}

#[test]
fn mlb_cm_err() {
  let e = check_empty_cm(&["foo.mlb", "foo.cm"], None).unwrap_err();
  assert!(e.to_string().contains("multiple root group files"));
}

#[test]
fn mlb_cm_config_cm_ok() {
  let config = r#"
version = 1
[workspace]
root = "foo.cm"
"#;
  check_input([("foo.mlb", ""), ("foo.cm", "Group is")], Some(config)).unwrap();
}

#[test]
fn mlb_cm_config_mlb_ok() {
  let config = r#"
version = 1
workspace.root = "foo.mlb"
"#;
  check_input([("foo.mlb", ""), ("foo.cm", "Group is")], Some(config)).unwrap();
}

#[test]
fn errors_severity() {
  let config = r#"
version = 1
[errors]
1001.severity = "error"
1002.severity = "warning"
1003.severity = "ignore"
"#;
  check_empty_cm(&["foo.cm"], Some(config)).unwrap();
}

#[test]
fn errors_severity_unknown() {
  let config = r#"
version = 1
[errors]
1001.severity = "Warning"
"#;
  let e = check_empty_cm(&["foo.cm"], Some(config)).unwrap_err();
  assert!(e.to_string().contains("unknown variant `Warning`"));
}

#[test]
fn std_basis_export() {
  let contents = r#"
Library
  library($/basis.cm)
is
  $/basis.cm
"#;
  check_input([("sources.cm", contents)], None).unwrap();
}

#[test]
fn std_basis_group() {
  let contents = r#"
Library
  group($/basis.cm)
is
  $/basis.cm
"#;
  let e = check_input([("sources.cm", contents)], None).unwrap_err();
  assert!(e.to_string().contains("expected a regular path or `-`"));
}

#[test]
fn cm_ident() {
  let contents = r#"
Library
  structure FOO_BAR_QUZ
  signature F__13123123123_FOO_BAR435QUZ6345FOO_BAR____WTF____1234234
is
"#;
  check_input([("sources.cm", contents)], None).unwrap();
}

#[test]
fn mlb_ident() {
  let contents = r#"
structure FOO_BAR_QUZ
signature F__13123123123_FOO_BAR435QUZ6345FOO_BAR____WTF____1234234
"#;
  check_input([("sources.mlb", contents)], None).unwrap();
}

#[test]
fn cm_union() {
  let contents = r#"
Library
  structure FOO
  (
    signature BAR
    signature QUZ
  )
is
"#;
  check_input([("sources.cm", contents)], None).unwrap();
}

#[test]
fn cm_intersection() {
  let contents = r#"
Library
  structure FOO * signature BAR
is
"#;
  let e = check_input([("sources.cm", contents)], None).unwrap_err();
  assert!(e.to_string().contains("unsupported export kind"));
}

#[test]
fn cm_difference() {
  let contents = r#"
Library
  structure FOO - signature BAR
is
"#;
  let e = check_input([("sources.cm", contents)], None).unwrap_err();
  assert!(e.to_string().contains("unsupported export kind"));
}

#[test]
fn cm_source_file() {
  let contents = r#"
Library
  source(foo.sml)
is
"#;
  let e = check_input([("sources.cm", contents)], None).unwrap_err();
  assert!(e.to_string().contains("unsupported export kind"));
}

#[test]
fn cm_source_dash() {
  let contents = r#"
Library
  source(-)
is
"#;
  let e = check_input([("sources.cm", contents)], None).unwrap_err();
  assert!(e.to_string().contains("unsupported export kind"));
}

fn check_empty_cm(
  names: &[&str],
  config: Option<&str>,
) -> Result<analysis::input::Input, analysis::input::Error> {
  check_input(names.iter().map(|&x| (x, "Group is")), config)
}

fn check_input<'a, I>(
  groups: I,
  config: Option<&str>,
) -> Result<analysis::input::Input, analysis::input::Error>
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  let fs = paths::MemoryFileSystem::new(
    groups
      .into_iter()
      .map(|(name, contents)| (ROOT.as_path().join(name), contents.to_owned()))
      .chain(config.map(|x| (ROOT.as_path().join(config::FILE_NAME), x.to_owned())))
      .collect(),
  );
  let mut store = paths::Store::new();
  analysis::input::Input::new(&fs, &mut store, &ROOT)
}
