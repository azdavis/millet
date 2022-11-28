//! Tests about the repository itself.

use fast_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeSet;
use std::fmt::{self, Write as _};
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
  assert!(only_lhs_set.is_empty(), "{only_lhs}: {only_lhs_set:#?}");
  let only_rhs_set: Vec<_> = rhs.difference(lhs).collect();
  assert!(only_rhs_set.is_empty(), "{only_rhs}: {only_rhs_set:#?}");
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
fn sml_def() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let dirs: [PathBuf; 3] =
    ["sml-hir", "sml-lower", "sml-statics"].map(|x| ["crates", x, "src"].iter().collect());
  let out = cmd!(sh, "git grep -hoE '@def\\(([[:digit:]]+)\\)' {dirs...}").output().unwrap();
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
fn sync() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let out = cmd!(sh, "git grep -hoE '@sync\\(([a-z_]+)\\)'").output().unwrap();
  let out = String::from_utf8(out.stdout).unwrap();
  let iter = out.lines().filter_map(|line| {
    let (_, inner) = line.split_once("@sync(")?;
    let (s, _) = inner.split_once(')')?;
    Some(s)
  });
  let mut map = FxHashMap::<&str, usize>::default();
  for x in iter {
    *map.entry(x).or_default() += 1;
  }
  map.retain(|_, &mut v| v != 2);
  assert!(map.is_empty(), "some sync comments occurred not exactly twice: {map:?}");
}

#[test]
fn test_refs() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let dir: PathBuf = ["crates", "sml-statics", "src"].into_iter().collect();
  let out = cmd!(sh, "git grep -hoE '@test\\(([a-z0-9_:]+)\\)' {dir}").output().unwrap();
  let out = String::from_utf8(out.stdout).unwrap();
  let referenced: BTreeSet<_> = out
    .lines()
    .filter_map(|line| {
      let (_, inner) = line.split_once("@test(")?;
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
fn architecture() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let in_doc: BTreeSet<_> = include_str!("../../../docs/ARCHITECTURE.md")
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
  let in_doc: BTreeSet<_> = include_str!("../../../docs/README.md")
    .lines()
    .filter_map(|x| {
      let x = x.strip_prefix("- [")?;
      let (_, x) = x.split_once("](./")?;
      let (x, _) = x.split_once("):")?;
      Some(x.to_owned())
    })
    .collect();
  let on_fs: BTreeSet<_> = sh
    .read_dir("docs")
    .unwrap()
    .into_iter()
    .filter_map(|x| {
      let x = x.file_name()?.to_str()?;
      (x != "README.md").then(|| x.to_owned())
    })
    .collect();
  eq_sets(&in_doc, &on_fs, "in README, but doesn't exist", "not in README, but exists");
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
  let entries: BTreeSet<_> = include_str!("../../../docs/CHANGELOG.md")
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

/// deprecated (do not use, but keep in this comment so it'll be picked up by the `git grep`):
/// - `Code::n(5007)`
#[test]
fn error_codes() {
  let sh = Shell::new().unwrap();
  sh.change_dir(root_dir());
  let output = cmd!(sh, "git grep -hoE 'Code::n\\([[:digit:]]+\\)'").read().unwrap();
  let in_doc = no_dupes(
    include_str!("../../../docs/diagnostics.md")
      .lines()
      .filter_map(|line| Some(line.strip_prefix("## ")?.parse::<u16>().unwrap())),
  );
  let in_code = no_dupes(output.lines().map(|line| {
    line.strip_prefix("Code::n(").unwrap().strip_suffix(')').unwrap().parse::<u16>().unwrap()
  }));
  eq_sets(&in_doc, &in_code, "diagnostics documented but not used", "diagnostics not documented");
}

#[derive(Debug)]
struct EnumVariant<'a> {
  name: &'a str,
  desc: &'a str,
}

impl fmt::Display for EnumVariant<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "`{:?}`: {}", self.name, self.desc)
  }
}

#[derive(Debug)]
enum TypeAndDefault<'a> {
  Bool(bool),
  String(&'a str, Vec<EnumVariant<'a>>),
}

impl fmt::Display for TypeAndDefault<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TypeAndDefault::Bool(b) => {
        writeln!(f, "- Type: `boolean`")?;
        writeln!(f, "- Default: `{b:?}`")?;
        Ok(())
      }
      TypeAndDefault::String(s, vs) => {
        writeln!(f, "- Type: `string`")?;
        writeln!(f, "- Default: `{s:?}`")?;
        if !vs.is_empty() {
          writeln!(f, "- Valid values:")?;
          for v in vs {
            writeln!(f, "  - {v}")?;
          }
        }
        Ok(())
      }
    }
  }
}

#[derive(Debug)]
struct ConfigProperty<'a> {
  name: &'a str,
  desc: &'a str,
  type_and_default: TypeAndDefault<'a>,
}

impl fmt::Display for ConfigProperty<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    writeln!(f, "#### `{}`", self.name)?;
    writeln!(f)?;
    writeln!(f, "{}", self.desc)?;
    writeln!(f)?;
    self.type_and_default.fmt(f)?;
    Ok(())
  }
}

#[test]
fn vs_code_config() {
  let package_json = include_str!("../../../editors/vscode/package.json");
  let package_json: serde_json::Value = serde_json::from_str(package_json).unwrap();
  let properties = package_json
    .as_object()
    .unwrap()
    .get("contributes")
    .unwrap()
    .as_object()
    .unwrap()
    .get("configuration")
    .unwrap()
    .as_object()
    .unwrap()
    .get("properties")
    .unwrap()
    .as_object()
    .unwrap();
  let mut want_doc = String::new();
  for (name, val) in properties {
    let val = val.as_object().unwrap();
    let typ = val.get("type").unwrap().as_str().unwrap();
    let default = val.get("default").unwrap();
    let desc = val.get("markdownDescription").unwrap().as_str().unwrap();
    let enums = val.get("enum").and_then(serde_json::Value::as_array);
    let enum_descs = val.get("markdownEnumDescriptions").and_then(serde_json::Value::as_array);
    assert_eq!(enums.map(Vec::len), enum_descs.map(Vec::len));
    let enum_variants: Vec<_> = enums
      .into_iter()
      .flatten()
      .zip(enum_descs.into_iter().flatten())
      .map(|(name, desc)| EnumVariant {
        name: name.as_str().unwrap(),
        desc: desc.as_str().unwrap(),
      })
      .collect();
    let type_and_default = match typ {
      "boolean" => {
        assert!(enum_variants.is_empty());
        TypeAndDefault::Bool(default.as_bool().unwrap())
      }
      "string" => TypeAndDefault::String(default.as_str().unwrap(), enum_variants),
      _ => panic!("unknown type: {typ}"),
    };
    let config_property = ConfigProperty { name, desc, type_and_default };
    writeln!(want_doc, "{config_property}").unwrap();
  }
  let manual = include_str!("../../../docs/manual.md");
  let mut iter = manual.lines();
  iter.find(|x| x.trim() == "<!-- @begin vscode-config -->");
  let got_doc_lines: Vec<_> =
    iter.take_while(|x| x.trim() != "<!-- @end vscode-config -->").collect();
  let got_doc = got_doc_lines.join("\n");
  pretty_assertions::assert_str_eq!(got_doc.trim(), want_doc.trim(),);
}
