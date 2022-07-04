//! Low-level tests for [`analysis::get_input`].

use crate::check::ROOT;

#[test]
fn arbitrary_root_group() {
  check_groups(&["foo.cm"]).unwrap();
}

#[test]
fn no_root_group() {
  let e = check_groups(&[]).unwrap_err();
  assert!(e.to_string().contains("no root group"));
}

#[test]
fn multiple_root_groups() {
  let e = check_groups(&["foo.cm", "bar.cm"]).unwrap_err();
  assert!(e.to_string().contains("multiple root groups"));
}

fn check_groups(names: &[&str]) -> Result<analysis::Input, analysis::GetInputError> {
  let fs = paths::MemoryFileSystem::new(
    names
      .iter()
      .map(|&name| (ROOT.as_path().join(name), "Group is".to_owned()))
      .collect(),
  );
  let mut root = paths::Root::new(ROOT.to_owned());
  analysis::get_input(&fs, &mut root)
}
