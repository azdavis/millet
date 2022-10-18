//! Tests about the repository itself.

use fast_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use xshell::{cmd, Shell};

fn root_dir() -> &'static Path {
  Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().parent().unwrap()
}

fn eq_sets<T>(lhs: &BTreeSet<T>, rhs: &BTreeSet<T>, only_lhs: &str, only_rhs: &str)
where
  T: Ord + std::fmt::Debug,
{
  let only_lhs_set: Vec<_> = lhs.difference(rhs).collect();
  assert!(only_lhs_set.is_empty(), "{only_lhs}: {only_lhs_set:?}");
  let only_rhs_set: Vec<_> = rhs.difference(lhs).collect();
  assert!(only_rhs_set.is_empty(), "{only_rhs}: {only_rhs_set:?}");
}

#[test]
fn sml_def() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let dirs: [PathBuf; 3] =
    ["sml-hir", "sml-lower", "sml-statics"].map(|x| ["crates", x, "src"].iter().collect());
  let out = cmd!(sh, "git grep -hoE 'Def\\(([[:digit:]]+)\\)' {dirs...}").output().unwrap();
  let got: BTreeSet<u16> = String::from_utf8(out.stdout)
    .unwrap()
    .lines()
    .filter_map(|line| {
      let (_, inner) = line.split_once('(')?;
      let (num, _) = inner.split_once(')')?;
      num.parse().ok()
    })
    .collect();
  let want = 1u16..=89;
  let missing: Vec<_> = want.clone().filter(|x| !got.contains(x)).collect();
  assert!(missing.is_empty(), "missing sml definition references: {missing:?}");
  let extra: Vec<_> = got.iter().filter(|&x| !want.contains(x)).collect();
  assert!(extra.is_empty(), "extra sml definition references: {extra:?}");
}

#[test]
fn test_refs() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let dir: PathBuf = ["crates", "sml-statics", "src"].into_iter().collect();
  let out = cmd!(sh, "git grep -hoE 'test\\(([a-z0-9_:]+)\\)' {dir}").output().unwrap();
  let out = String::from_utf8(out.stdout).unwrap();
  let referenced: BTreeSet<_> = out
    .lines()
    .filter_map(|line| {
      let (_, inner) = line.split_once("// test(")?;
      let (name, _) = inner.split_once(')')?;
      Some(name)
    })
    .collect();
  let out = cmd!(sh, "cargo test -p tests -- --list").output().unwrap();
  let out = String::from_utf8(out.stdout).unwrap();
  let defined: BTreeSet<_> = out.lines().filter_map(|line| line.strip_suffix(": test")).collect();
  let ref_not_defined: BTreeSet<_> = referenced.difference(&defined).copied().collect();
  assert!(ref_not_defined.is_empty(), "tests referenced but not defined: {ref_not_defined:?}");
}

#[test]
fn no_ignore() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let word = "ignore";
  let has_ignore = cmd!(sh, "git grep -lFe #[{word}").ignore_status().output().unwrap();
  let out = String::from_utf8(has_ignore.stdout).unwrap();
  let set: BTreeSet<_> = out.lines().collect();
  assert!(set.is_empty(), "found files with {word}: {set:?}");
}

#[test]
fn crate_architecture_doc() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let in_doc: BTreeSet<_> = include_str!("../../../docs/architecture.md")
    .lines()
    .filter_map(|line| Some(line.strip_prefix("### `crates/")?.strip_suffix('`')?.to_owned()))
    .collect();
  let in_crates: BTreeSet<_> = sh
    .read_dir("crates")
    .unwrap()
    .into_iter()
    .filter_map(|x| Some(x.file_name()?.to_str()?.to_owned()))
    .collect();
  eq_sets(
    &in_doc,
    &in_crates,
    "documented crates that don't exist",
    "crates without documentation",
  );
}

#[test]
fn docs_readme() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let in_readme: BTreeSet<_> = include_str!("../../../docs/readme.md")
    .lines()
    .filter_map(|x| {
      let x = x.strip_prefix("- [")?;
      let (_, x) = x.split_once("](./")?;
      let (x, _) = x.split_once("):")?;
      Some(x.to_owned())
    })
    .collect();
  let in_dir: BTreeSet<_> = sh
    .read_dir("docs")
    .unwrap()
    .into_iter()
    .filter_map(|x| {
      let x = x.file_name()?.to_str()?;
      (x != "readme.md").then(|| x.to_owned())
    })
    .collect();
  eq_sets(&in_readme, &in_dir, "in readme, but doesn't exist", "not in readme, but exists");
}

#[test]
fn no_debugging() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  // the uppercase + to_ascii_lowercase is to prevent git grep from triggering on this file.
  let fst = "DBG".to_ascii_lowercase();
  let snd = "EPRINT".to_ascii_lowercase();
  let thd = "CONSOLE.LOG".to_ascii_lowercase();
  // ignore status because if no results (which is desired), git grep returns non-zero.
  let got =
    cmd!(sh, "git grep -F -n -e {fst} -e {snd} -e {thd}").ignore_status().output().unwrap().stdout;
  let got = String::from_utf8(got).unwrap();
  let got: BTreeSet<_> = got.lines().collect();
  eq_sets(&BTreeSet::new(), &got, "expected to have debugging", "not allowed to have debugging");
}

#[test]
fn changelog() {
  if option_env!("CI") == Some("1") {
    return;
  }
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let tag_out = String::from_utf8(cmd!(sh, "git tag").output().unwrap().stdout).unwrap();
  let tags: BTreeSet<_> = tag_out.lines().filter(|x| x.starts_with('v')).collect();
  let entries: BTreeSet<_> = include_str!("../../../docs/changelog.md")
    .lines()
    .filter_map(|line| {
      let title = line.strip_prefix("## ")?;
      // allow a title of 'main' for unreleased changes.
      (title != "main").then_some(title)
    })
    .collect();
  eq_sets(&tags, &entries, "tags that have no changelog entry", "changelog entries without a tag");
}

#[test]
fn licenses() {
  let allowed = fast_hash::set([
    "(MIT OR Apache-2.0) AND Unicode-DFS-2016",
    "Apache-2.0 OR BSL-1.0",
    "Apache-2.0 OR MIT",
    "Apache-2.0/MIT",
    "MIT",
    "MIT OR Apache-2.0",
    "MIT OR Apache-2.0 OR Zlib",
    "MIT/Apache-2.0",
    "Unlicense OR MIT",
    "Unlicense/MIT",
    "Zlib OR Apache-2.0 OR MIT",
  ]);
  let sh = Shell::new().unwrap();
  let output = cmd!(sh, "cargo metadata --format-version 1").read().unwrap();
  let json: serde_json::Value = serde_json::from_str(&output).unwrap();
  let packages = json.as_object().unwrap().get("packages").unwrap().as_array().unwrap();
  let mut new_licenses = FxHashMap::<&str, FxHashSet<&str>>::default();
  for package in packages {
    let package = package.as_object().unwrap();
    let license = package.get("license").unwrap().as_str().unwrap();
    if allowed.contains(&license) {
      continue;
    }
    let name = package.get("name").unwrap().as_str().unwrap();
    new_licenses.entry(license).or_default().insert(name);
  }
  for (license, names) in &new_licenses {
    println!("{license}: {names:?}");
  }
  assert!(new_licenses.is_empty(), "found {} new licenses", new_licenses.len());
}

fn no_dupes<I, T>(iter: I) -> BTreeSet<T>
where
  I: Iterator<Item = T>,
  T: Ord + Copy + std::fmt::Display,
{
  let mut ret = BTreeSet::<T>::default();
  for x in iter {
    assert!(ret.insert(x), "duplicate: {x}");
  }
  ret
}

#[test]
fn error_codes() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let output = cmd!(sh, "git grep -hoE 'Code::n\\([[:digit:]]+\\)'").read().unwrap();
  let in_doc = no_dupes(
    include_str!("../../../docs/errors.md")
      .lines()
      .filter_map(|line| Some(line.strip_prefix("## ")?.parse::<u16>().unwrap())),
  );
  let in_code = no_dupes(output.lines().map(|line| {
    line.strip_prefix("Code::n(").unwrap().strip_suffix(')').unwrap().parse::<u16>().unwrap()
  }));
  eq_sets(&in_doc, &in_code, "errors documented but not used", "errors not documented");
}
