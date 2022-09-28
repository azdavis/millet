//! Low-level tests for [`analysis::input::get`].

use crate::check::ROOT;

#[test]
fn arbitrary_root_group() {
  check_empty_cm(&["foo.cm"], None).unwrap();
}

#[test]
fn no_root_group() {
  let e = check_empty_cm(&[], None).unwrap_err();
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

fn check_empty_cm(
  names: &[&str],
  config: Option<&str>,
) -> Result<analysis::input::Input, analysis::input::GetInputError> {
  check_input(names.iter().map(|&x| (x, "Group is")), config)
}

fn check_input<'a, I>(
  groups: I,
  config: Option<&str>,
) -> Result<analysis::input::Input, analysis::input::GetInputError>
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
  let mut root = analysis::input::get_root_dir(ROOT.to_owned());
  analysis::input::get(&fs, &mut root)
}
