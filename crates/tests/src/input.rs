//! Low-level tests for [`analysis::get_input`].

use crate::check::ROOT;

#[test]
fn arbitrary_root_group() {
  check_input(&["foo.cm"], None).unwrap();
}

#[test]
fn no_root_group() {
  let e = check_input(&[], None).unwrap_err();
  assert!(e.to_string().contains("no root group"));
}

#[test]
fn multiple_root_groups_err() {
  let e = check_input(&["foo.cm", "bar.cm"], None).unwrap_err();
  assert!(e.to_string().contains("multiple root groups"));
}

#[test]
fn multiple_root_groups_ok() {
  let config = r#"
version = 1
[workspace]
# prefer foo over bar
root = "foo.cm"
"#;
  check_input(&["foo.cm", "bar.cm"], Some(config)).unwrap();
}

#[test]
fn no_root_group_in_config_ok() {
  check_input(&["sources.cm"], Some("version = 1")).unwrap();
}

#[test]
fn config_invalid_version() {
  let e = check_input(&["sources.cm"], Some("version = 123")).unwrap_err();
  assert!(e.to_string().contains("invalid config version"));
}

#[test]
fn config_path_not_exist() {
  let config = r#"
version = 1
[workspace]
root = "nope.cm"
"#;
  check_input(&["foo.cm"], Some(config)).unwrap_err();
}

#[test]
fn config_parse_err() {
  check_input(&["foo.cm"], Some("岡部倫太郎")).unwrap_err();
}

fn check_input(
  names: &[&str],
  config: Option<&str>,
) -> Result<analysis::Input, analysis::GetInputError> {
  let fs = paths::MemoryFileSystem::new(
    names
      .iter()
      .map(|&name| (ROOT.as_path().join(name), "Group is".to_owned()))
      .chain(config.map(|x| (ROOT.as_path().join(config::FILE_NAME), x.to_owned())))
      .collect(),
  );
  let mut root = paths::Root::new(ROOT.to_owned());
  analysis::get_input(&fs, &mut root, None)
}
