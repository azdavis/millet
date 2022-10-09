//! Tests about the repository itself.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use xshell::{cmd, Shell};

fn check<F>(f: F)
where
  F: FnOnce(&Shell),
{
  let sh = Shell::new().expect("couldn't make Shell");
  let _d = sh.push_dir(Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().parent().unwrap());
  f(&sh);
}

fn eq_sets<T>(lhs: &BTreeSet<T>, rhs: &BTreeSet<T>, only_lhs: &str, only_rhs: &str)
where
  T: Ord + std::fmt::Debug,
{
  let only_lhs_set: Vec<_> = lhs.difference(rhs).collect();
  if !only_lhs_set.is_empty() {
    panic!("{only_lhs}: {only_lhs_set:?}");
  }
  let only_rhs_set: Vec<_> = rhs.difference(lhs).collect();
  if !only_rhs_set.is_empty() {
    panic!("{only_rhs}: {only_rhs_set:?}");
  }
}

#[test]
fn sml_def() {
  check(|sh| {
    let dirs: [PathBuf; 3] =
      ["sml-hir", "sml-lower", "sml-statics"].map(|x| ["crates", x, "src"].iter().collect());
    let out = cmd!(sh, "git grep -hoE 'sml_def\\(([[:digit:]]+)\\)' {dirs...}").output().unwrap();
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
    if !missing.is_empty() {
      panic!("missing sml definition references: {missing:?}")
    }
    let extra: Vec<_> = got.iter().filter(|&x| !want.contains(x)).collect();
    if !extra.is_empty() {
      panic!("extra sml definition references: {extra:?}")
    }
  })
}

#[test]
fn test_refs() {
  check(|sh| {
    let dir: PathBuf = ["crates", "sml-statics", "src"].into_iter().collect();
    let out = cmd!(sh, "git grep -hoE 'test\\(([a-z0-9_:]+)\\)' {dir}").output().unwrap();
    let out = String::from_utf8(out.stdout).unwrap();
    let referenced: BTreeSet<_> = out
      .lines()
      .filter_map(|line| {
        let (_, inner) = line.split_once("test(")?;
        let (name, _) = inner.split_once(')')?;
        Some(name)
      })
      .collect();
    let out = cmd!(sh, "cargo test -p tests -- --list").output().unwrap();
    let out = String::from_utf8(out.stdout).unwrap();
    let defined: BTreeSet<_> = out.lines().filter_map(|line| line.strip_suffix(": test")).collect();
    let ref_not_defined: BTreeSet<_> = referenced.difference(&defined).copied().collect();
    if !ref_not_defined.is_empty() {
      panic!("tests referenced but not defined: {ref_not_defined:?}")
    }
  })
}

#[test]
fn no_ignore() {
  check(|sh| {
    let word = "ignore";
    println!("checking for no {word}");
    let has_ignore = cmd!(sh, "git grep -lFe #[{word}").ignore_status().output().unwrap();
    let out = String::from_utf8(has_ignore.stdout).unwrap();
    let set: BTreeSet<_> = out.lines().collect();
    if !set.is_empty() {
      panic!("found files with {word}: {set:?}");
    }
  })
}

#[test]
fn sml_libs() {
  check(|sh| {
    let mut path: PathBuf = ["crates", "sml-libs", "src"].into_iter().collect();
    let mut entries = sh.read_dir(&path).unwrap();
    entries.retain(|x| x.extension().is_none());
    for dir in entries {
      let dir_name = dir
        .file_name()
        .expect("no file name for dir")
        .to_str()
        .expect("cannot convert dirname to str");
      println!("- checking {dir_name}");
      let mod_path = dir.as_path().with_extension("rs");
      path.push(mod_path);
      let mod_file = sh.read_file(&path).unwrap();
      let prefix = format!("  \"{dir_name}/");
      let in_order: BTreeSet<_> =
        mod_file.lines().filter_map(|x| x.strip_prefix(&prefix)?.strip_suffix(".sml\",")).collect();
      assert!(path.pop());
      path.push(&dir);
      let sml_dir = sh.read_dir(&path).unwrap();
      let in_files: BTreeSet<_> = sml_dir
        .iter()
        .filter_map(|x| {
          if x.extension()? == "sml" {
            x.file_name()?.to_str()?.strip_suffix(".sml")
          } else {
            None
          }
        })
        .collect();
      eq_sets(
        &in_order,
        &in_files,
        "referenced files that don't exist",
        "existing files not referenced",
      );
    }
  })
}

#[test]
fn crate_architecture_doc() {
  check(|sh| {
    let path = sh.current_dir().join("docs").join("architecture.md");
    let contents = sh.read_file(path).unwrap();
    let in_doc: BTreeSet<_> = contents
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
  })
}

#[test]
fn docs_readme() {
  check(|sh| {
    let path = sh.current_dir().join("docs").join("readme.md");
    let contents = sh.read_file(path).unwrap();
    let in_readme: BTreeSet<_> = contents
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
  })
}

#[test]
fn no_debugging() {
  check(|sh| {
    // the uppercase + to_ascii_lowercase is to prevent git grep from triggering on this file.
    let fst = "DBG".to_ascii_lowercase();
    let snd = "EPRINT".to_ascii_lowercase();
    let thd = "CONSOLE.LOG".to_ascii_lowercase();
    // ignore status because if no results (which is desired), git grep returns non-zero.
    let got = cmd!(sh, "git grep -F -n -e {fst} -e {snd} -e {thd}")
      .ignore_status()
      .output()
      .unwrap()
      .stdout;
    let got = String::from_utf8(got).unwrap();
    let got: BTreeSet<_> = got.lines().collect();
    eq_sets(&BTreeSet::new(), &got, "expected to have debugging", "not allowed to have debugging");
  })
}

#[test]
fn changelog() {
  if option_env!("CI") == Some("1") {
    return;
  }
  check(|sh| {
    let tag_out = String::from_utf8(cmd!(sh, "git tag").output().unwrap().stdout).unwrap();
    let tags: BTreeSet<_> = tag_out.lines().filter(|x| x.starts_with('v')).collect();
    let path = sh.current_dir().join("docs").join("changelog.md");
    let contents = sh.read_file(path).unwrap();
    let entries: BTreeSet<_> = contents
      .lines()
      .filter_map(|line| {
        let title = line.strip_prefix("## ")?;
        // allow a title of 'main' for unreleased changes.
        (title != "main").then_some(title)
      })
      .collect();
    eq_sets(
      &tags,
      &entries,
      "tags that have no changelog entry",
      "changelog entries without a tag",
    );
  })
}
