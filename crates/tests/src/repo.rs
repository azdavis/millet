//! Tests about the repository itself.

use crate::check::raw::env_var_enabled;
use fast_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeSet;
use std::fmt::{self, Write as _};
use std::path::{Path, PathBuf};
use xshell::{cmd, Shell};

fn eq_sets<T>(lhs: &BTreeSet<T>, rhs: &BTreeSet<T>, only_lhs: &str, only_rhs: &str)
where
  T: Ord + std::fmt::Debug,
{
  let only_lhs_set: Vec<_> = lhs.difference(rhs).collect();
  assert!(only_lhs_set.is_empty(), "{only_lhs}: {only_lhs_set:#?}");
  let only_rhs_set: Vec<_> = rhs.difference(lhs).collect();
  assert!(only_rhs_set.is_empty(), "{only_rhs}: {only_rhs_set:#?}");
}

fn empty_set<T>(set: &BTreeSet<T>, msg: &str)
where
  T: Ord + std::fmt::Debug,
{
  assert!(set.is_empty(), "{msg}: {set:#?}");
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

fn shell() -> Shell {
  let sh = Shell::new().unwrap();
  let root_dir = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().parent().unwrap();
  sh.change_dir(root_dir);
  sh
}

#[test]
fn sml_def() {
  let sh = shell();
  let dirs: [PathBuf; 3] =
    ["sml-hir", "sml-lower", "sml-statics"].map(|x| ["crates", x, "src"].iter().collect());
  let out = cmd!(sh, "git grep -h -o -E '@def\\(([[:digit:]]+)\\)' {dirs...}").output().unwrap();
  let got: BTreeSet<u16> = String::from_utf8(out.stdout)
    .unwrap()
    .lines()
    .filter_map(|line| {
      let (_, inner) = line.split_once('(')?;
      let (num, _) = inner.split_once(')')?;
      num.parse().ok()
    })
    .collect();
  let want: BTreeSet<_> = (1u16..=89).collect();
  eq_sets(&want, &got, "missing sml definition references", "extra sml definition references");
}

#[test]
fn sync() {
  let sh = shell();
  let out = cmd!(sh, "git grep -h -o -E '@sync\\(([a-z_]+)\\)'").output().unwrap();
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
  let not_twice: BTreeSet<_> = map.iter().filter_map(|(&k, &v)| (v != 2).then_some(k)).collect();
  empty_set(&not_twice, "sync comments occurred not exactly twice");
}

#[test]
fn test_refs() {
  let sh = shell();
  let out = cmd!(sh, "git grep -h -o -E '@test\\(([a-z0-9_:]+)\\)'").output().unwrap();
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
  empty_set(&ref_not_defined, "tests referenced but not defined");
}

fn attr_test(word: &str) {
  let sh = shell();
  let out = cmd!(sh, "git grep -F -n -e #[{word}").ignore_status().output().unwrap();
  let out = String::from_utf8(out.stdout).unwrap();
  let set: BTreeSet<_> = out.lines().collect();
  empty_set(&set, format!("files with {word} attribute").as_str());
}

#[test]
fn no_ignore() {
  attr_test("ignore");
}

#[test]
fn architecture() {
  let sh = shell();
  let in_doc: BTreeSet<_> = include_str!("../../../docs/ARCHITECTURE.md")
    .lines()
    .filter_map(|line| Some(line.strip_prefix("### `")?.strip_suffix('`')?.to_owned()))
    .collect();
  let mut no_doc = BTreeSet::from(["crates", "LICENSE-APACHE.md", "LICENSE-MIT.md", "README.md"]);
  let on_fs: BTreeSet<_> = std::iter::empty()
    .chain(sh.read_dir("crates").unwrap().into_iter().filter_map(|x| {
      let file_name = x.file_name()?.to_str()?;
      Some(format!("crates/{file_name}"))
    }))
    .chain(sh.read_dir("editors").unwrap().into_iter().filter_map(|x| {
      let file_name = x.file_name()?.to_str()?;
      Some(format!("editors/{file_name}"))
    }))
    .chain(
      String::from_utf8(cmd!(sh, "git ls-tree --name-only HEAD").output().unwrap().stdout)
        .unwrap()
        .lines()
        .filter_map(|x| if no_doc.remove(x) { None } else { Some(x.to_owned()) }),
    )
    .collect();
  empty_set(&no_doc, "explicitly non-documented items not found on fs");
  eq_sets(&in_doc, &on_fs, "documented items that don't exist", "items without documentation");
}

#[test]
fn docs_readme() {
  let sh = shell();
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
  let sh = shell();
  // the uppercase + to_ascii_lowercase is to prevent git grep from triggering on this file.
  let fst = "DBG".to_ascii_lowercase();
  let snd = "EPRINT".to_ascii_lowercase();
  let thd = "CONSOLE.LOG".to_ascii_lowercase();
  // ignore status because if no results (which is desired), git grep returns non-zero.
  let got =
    cmd!(sh, "git grep -F -n -e {fst} -e {snd} -e {thd}").ignore_status().output().unwrap().stdout;
  let got = String::from_utf8(got).unwrap();
  let got: BTreeSet<_> = got.lines().collect();
  empty_set(&got, "not allowed to have debugging");
}

#[test]
fn changelog() {
  if env_var_enabled("CI") {
    return;
  }
  let sh = shell();
  let tag_out = String::from_utf8(cmd!(sh, "git tag").output().unwrap().stdout).unwrap();
  let in_git: BTreeSet<_> = tag_out.lines().filter(|x| x.starts_with('v')).collect();
  let in_doc: BTreeSet<_> = include_str!("../../../docs/CHANGELOG.md")
    .lines()
    .filter_map(|line| {
      let title = line.strip_prefix("## ")?;
      // allow a title of 'main' for unreleased changes.
      (title != "main").then_some(title)
    })
    .collect();
  eq_sets(&in_git, &in_doc, "tags that have no changelog entry", "changelog entries without a tag");
}

#[test]
fn licenses() {
  let allowed = fast_hash::set([
    "(MIT OR Apache-2.0) AND Unicode-DFS-2016",
    "Apache-2.0 OR BSL-1.0",
    "Apache-2.0 OR MIT",
    "Apache-2.0 WITH LLVM-exception OR Apache-2.0 OR MIT",
    "Apache-2.0/MIT",
    "MIT OR Apache-2.0 OR Zlib",
    "MIT OR Apache-2.0",
    "MIT",
    "MIT/Apache-2.0",
    "Unlicense OR MIT",
    "Unlicense/MIT",
    "Zlib OR Apache-2.0 OR MIT",
  ]);
  let sh = shell();
  let output = cmd!(sh, "cargo metadata --format-version 1").ignore_stderr().read().unwrap();
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

#[test]
fn diagnostic_codes() {
  let sh = shell();
  let output = cmd!(sh, "git grep -h -o -E 'Code::n\\([[:digit:]]+\\)'").read().unwrap();
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

const PACKAGE_JSON: &str = include_str!("../../../editors/vscode/package.json");
const MANUAL: &str = include_str!("../../../docs/manual.md");
const COMMENT_CLOSE: &str = " -->";

fn is_section_comment(s: &str, comment_open: &str, section: &str) -> bool {
  s.trim()
    .strip_prefix(comment_open)
    .and_then(|s| s.strip_suffix(COMMENT_CLOSE))
    .map_or(false, |s| s == section)
}

fn get_manual_section(section: &str) -> impl Iterator<Item = &'static str> + '_ {
  let mut iter = MANUAL.lines();
  iter.find(|s| is_section_comment(s, "<!-- @begin ", section));
  iter.take_while(|s| !is_section_comment(s, "<!-- @end ", section))
}

#[test]
fn vs_code_config() {
  let package_json: serde_json::Value = serde_json::from_str(PACKAGE_JSON).unwrap();
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
    let default_ = val.get("default").unwrap();
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
        TypeAndDefault::Bool(default_.as_bool().unwrap())
      }
      "string" => TypeAndDefault::String(default_.as_str().unwrap(), enum_variants),
      _ => panic!("unknown type: {typ}"),
    };
    let config_property = ConfigProperty { name, desc, type_and_default };
    writeln!(want_doc, "{config_property}").unwrap();
  }
  let got_doc_lines: Vec<_> = get_manual_section("vscode-config").collect();
  let got_doc = got_doc_lines.join("\n");
  pretty_assertions::assert_str_eq!(got_doc.trim(), want_doc.trim(),);
}

#[test]
fn node_version() {
  let version = "18";
  let package_json: serde_json::Value = serde_json::from_str(PACKAGE_JSON).unwrap();
  let build_base = package_json
    .as_object()
    .unwrap()
    .get("scripts")
    .unwrap()
    .as_object()
    .unwrap()
    .get("build-base")
    .unwrap()
    .as_str()
    .unwrap();
  let mut split = build_base.split("--target=node");
  split.next().unwrap();
  assert_eq!(split.next().unwrap(), version);
  assert!(split.next().is_none());
  let readme = include_str!("../../../README.md");
  let readme_table_line =
    readme.lines().find_map(|line| line.strip_prefix("| [Node.js][node]")).unwrap();
  let mut split = readme_table_line.split_ascii_whitespace();
  assert_eq!(split.next().unwrap(), "|");
  assert_eq!(split.next().unwrap(), version);
  let ci = include_str!("../../../.github/workflows/ci.yaml");
  let ci_version = ci
    .lines()
    .filter_map(|line| {
      let (_, rest) = line.split_once("node-version: ")?;
      let (v, _) = rest.split_once(".x")?;
      Some(v)
    })
    .all(|v| v == version);
  assert!(ci_version);
}

#[test]
fn rs_file_comments() {
  let sh = shell();
  let files =
    String::from_utf8(cmd!(sh, "git ls-files '**/*.rs'").output().unwrap().stdout).unwrap();
  let no_doc: BTreeSet<_> = files
    .lines()
    .filter_map(|file| {
      let out = sh.read_file(file).unwrap();
      let fst = out.lines().next().unwrap();
      (!fst.starts_with("//! ")).then_some(file)
    })
    .collect();
  empty_set(&no_doc, "rust files without doc comment at top");
}

#[test]
fn primitives() {
  let mut lines = include_str!("../../../crates/sml-statics/src/def.rs").lines();
  for line in lines.by_ref() {
    if line.trim() == "// @primitives(start)" {
      break;
    }
  }
  let mut in_rs = Vec::<&str>::new();
  for line in lines {
    let line = line.trim();
    if line == "// @primitives(end)" {
      break;
    }
    let (fst, _) = line.split_once("\" => ").unwrap();
    in_rs.push(fst.strip_prefix('"').unwrap());
  }
  let in_md: Vec<_> = include_str!("../../../docs/primitives.md")
    .lines()
    .filter_map(|line| line.strip_prefix("## `")?.strip_suffix('`'))
    .collect();
  pretty_assertions::assert_eq!(in_rs, in_md);
}

fn snippets(section: &str, json: &str) {
  let json: serde_json::Value = serde_json::from_str(json).unwrap();
  let from_json: BTreeSet<_> = json.as_object().unwrap().keys().map(String::as_str).collect();
  let from_manual: BTreeSet<_> =
    get_manual_section(section).filter_map(|s| s.strip_prefix("- ")).collect();
  eq_sets(&from_json, &from_manual, "snippets only in json", "snippets only documented in manual");
}

#[test]
fn mlb_snippets() {
  let json = include_str!("../../../editors/vscode/languages/mlb/snippets.json");
  snippets("mlb-snippets", json);
}

#[test]
fn sml_nj_cm_snippets() {
  let json = include_str!("../../../editors/vscode/languages/sml-nj-cm/snippets.json");
  snippets("sml-nj-cm-snippets", json);
}

#[test]
fn sml_snippets() {
  let json = include_str!("../../../editors/vscode/languages/sml/snippets.json");
  snippets("sml-snippets", json);
}
