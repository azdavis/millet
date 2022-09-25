//! See the [xtask spec](https://github.com/matklad/cargo-xtask).

use anyhow::{anyhow, bail, Result};
use pico_args::Arguments;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use xshell::{cmd, Shell};

#[derive(Debug, Clone, Copy)]
enum Cmd {
  Help,
  Ci,
  Dist,
  Tag,
}

struct CmdSpec {
  name: &'static str,
  desc: &'static str,
  options: &'static [(&'static str, &'static str)],
  args: &'static [(&'static str, &'static str)],
}

impl Cmd {
  const VALUES: [Cmd; 4] = [Cmd::Help, Cmd::Ci, Cmd::Dist, Cmd::Tag];

  fn spec(&self) -> CmdSpec {
    match self {
      Cmd::Help => CmdSpec { name: "help", desc: "show this help", options: &[], args: &[] },
      Cmd::Ci => CmdSpec { name: "ci", desc: "run various tests", options: &[], args: &[] },
      Cmd::Dist => CmdSpec {
        name: "dist",
        desc: "make artifacts for distribution",
        options: &[
          ("--release", "build for release"),
          ("--target <target>", "build for the <target> triple"),
        ],
        args: &[],
      },
      Cmd::Tag => CmdSpec {
        name: "tag",
        desc: "update package files with a new version, then commit a new tag",
        options: &[],
        args: &[("<tag>", "the name of the tag")],
      },
    }
  }
}

impl std::str::FromStr for Cmd {
  type Err = anyhow::Error;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Cmd::VALUES
      .iter()
      .find(|c| c.spec().name == s)
      .copied()
      .ok_or_else(|| anyhow!("couldn't parse {s} into a command"))
  }
}

fn show_help() {
  println!("usage:");
  println!("  cargo xtask <command> [<options>] [<args>]");
  println!();
  println!("commands:");
  for c in Cmd::VALUES {
    let spec = c.spec();
    println!("  {}", spec.name);
    println!("    {}", spec.desc);
    if !spec.options.is_empty() {
      println!();
      println!("    options:");
      for (name, desc) in spec.options {
        println!("      {name}");
        println!("        {desc}");
      }
    }
    if !spec.args.is_empty() {
      println!();
      println!("    args:");
      for (name, desc) in spec.args {
        println!("      {name}");
        println!("        {desc}");
      }
    }
  }
}

fn finish_args(args: Arguments) -> Result<()> {
  let args = args.finish();
  if !args.is_empty() {
    bail!("unused arguments: {args:?}")
  }
  Ok(())
}

fn ck_sml_def(sh: &Shell) -> Result<()> {
  println!("checking sml definition comments");
  let dirs: [PathBuf; 3] =
    ["sml-hir", "sml-lower", "sml-statics"].map(|x| ["crates", x, "src"].iter().collect());
  let out = cmd!(sh, "git grep -hoE 'sml_def\\(([[:digit:]]+)\\)' {dirs...}").output()?;
  let got: BTreeSet<u16> = String::from_utf8(out.stdout)?
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
    bail!("missing sml definition references: {missing:?}")
  }
  let extra: Vec<_> = got.iter().filter(|&x| !want.contains(x)).collect();
  if !extra.is_empty() {
    bail!("extra sml definition references: {extra:?}")
  }
  Ok(())
}

fn ck_test_refs(sh: &Shell) -> Result<()> {
  println!("checking test reference comments");
  let dir: PathBuf = ["crates", "sml-statics", "src"].into_iter().collect();
  let out = cmd!(sh, "git grep -hoE 'test\\(([a-z0-9_:]+)\\)' {dir}").output()?;
  let out = String::from_utf8(out.stdout)?;
  let referenced: BTreeSet<_> = out
    .lines()
    .filter_map(|line| {
      let (_, inner) = line.split_once("test(")?;
      let (name, _) = inner.split_once(')')?;
      Some(name)
    })
    .collect();
  let out = cmd!(sh, "cargo test -p tests -- --list").output()?;
  let out = String::from_utf8(out.stdout)?;
  let defined: BTreeSet<_> = out.lines().filter_map(|line| line.strip_suffix(": test")).collect();
  let ref_not_defined: BTreeSet<_> = referenced.difference(&defined).copied().collect();
  if ref_not_defined.is_empty() {
    Ok(())
  } else {
    bail!("tests referenced but not defined: {ref_not_defined:?}")
  }
}

fn ck_no_ignore(sh: &Shell) -> Result<()> {
  // avoid matching this file with the git grep
  let word = "ignore";
  println!("checking for no {word}");
  let has_ignore = cmd!(sh, "git grep -lFe #[{word}").ignore_status().output()?;
  let out = String::from_utf8(has_ignore.stdout)?;
  let set: BTreeSet<_> = out.lines().collect();
  if set.is_empty() {
    Ok(())
  } else {
    bail!("found files with {word}: {set:?}");
  }
}

fn eq_sets<T>(lhs: &BTreeSet<T>, rhs: &BTreeSet<T>, only_lhs: &str, only_rhs: &str) -> Result<()>
where
  T: Ord + std::fmt::Debug,
{
  let only_lhs_set: Vec<_> = lhs.difference(rhs).collect();
  if !only_lhs_set.is_empty() {
    bail!("{only_lhs}: {only_lhs_set:?}");
  }
  let only_rhs_set: Vec<_> = rhs.difference(lhs).collect();
  if !only_rhs_set.is_empty() {
    bail!("{only_rhs}: {only_rhs_set:?}");
  }
  Ok(())
}

fn ck_sml_libs(sh: &Shell) -> Result<()> {
  println!("checking sml libraries");
  let mut sml: PathBuf = ["crates", "sml-libs", "src", "std_basis", "mod.rs"].into_iter().collect();
  let order_file = sh.read_file(&sml)?;
  let in_order: BTreeSet<_> =
    order_file.lines().filter_map(|x| x.strip_prefix("  \"")?.strip_suffix(".sml\",")).collect();
  assert!(sml.pop());
  let sml_dir = sh.read_dir(&sml)?;
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
  )?;
  Ok(())
}

fn ck_crate_architecture_doc(sh: &Shell) -> Result<()> {
  println!("checking for crate architecture doc");
  let path = sh.current_dir().join("docs").join("architecture.md");
  let contents = sh.read_file(path)?;
  let in_doc: BTreeSet<_> = contents
    .lines()
    .filter_map(|line| line.strip_prefix("### `crates/")?.strip_suffix('`'))
    .map(ToOwned::to_owned)
    .collect();
  let in_crates: BTreeSet<_> = sh
    .read_dir("crates")?
    .into_iter()
    .filter_map(|x| Some(x.file_name()?.to_str()?.to_owned()))
    .collect();
  eq_sets(
    &in_doc,
    &in_crates,
    "documented crates that don't exist",
    "crates without documentation",
  )?;
  Ok(())
}

fn ck_docs_readme(sh: &Shell) -> Result<()> {
  println!("checking docs readme");
  let path = sh.current_dir().join("docs").join("readme.md");
  let contents = sh.read_file(path)?;
  let in_readme: BTreeSet<_> = contents
    .lines()
    .filter_map(|x| {
      let x = x.strip_prefix("- [")?;
      let (_, x) = x.split_once("](./")?;
      let (x, _) = x.split_once("):")?;
      Some(x)
    })
    .map(ToOwned::to_owned)
    .collect();
  let in_dir: BTreeSet<_> = sh
    .read_dir("docs")?
    .into_iter()
    .filter_map(|x| {
      let x = x.file_name()?.to_str()?;
      (x != "readme.md").then(|| x.to_owned())
    })
    .collect();
  eq_sets(&in_readme, &in_dir, "in readme, but doesn't exist", "not in readme, but exists")?;
  Ok(())
}

fn ck_no_debugging(sh: &Shell) -> Result<()> {
  // the uppercase + to_ascii_lowercase is to prevent git grep from triggering on this file.
  let fst = "DBG".to_ascii_lowercase();
  let snd = "EPRINT".to_ascii_lowercase();
  let thd = "CONSOLE.LOG".to_ascii_lowercase();
  // ignore status because if no results (which is desired), git grep returns non-zero.
  let got = cmd!(sh, "git grep -F -n -e {fst} -e {snd} -e {thd}").ignore_status().output()?.stdout;
  let got = String::from_utf8(got)?;
  let got: BTreeSet<_> = got.lines().collect();
  eq_sets(&BTreeSet::new(), &got, "expected to have debugging", "not allowed to have debugging")?;
  Ok(())
}

fn ck_changelog(sh: &Shell) -> Result<()> {
  println!("checking for changelog entries");
  let tag_out = String::from_utf8(cmd!(sh, "git tag").output()?.stdout)?;
  let tags: BTreeSet<_> = tag_out.lines().filter(|x| x.starts_with('v')).collect();
  let path = sh.current_dir().join("docs").join("changelog.md");
  let contents = sh.read_file(path)?;
  let entries: BTreeSet<_> = contents
    .lines()
    .filter_map(|line| {
      let title = line.strip_prefix("## ")?;
      // allow a title of 'main' for unreleased changes.
      (title != "main").then_some(title)
    })
    .collect();
  eq_sets(&tags, &entries, "tags that have no changelog entry", "changelog entries without a tag")?;
  Ok(())
}

fn dist(sh: &Shell, release: bool, target: Option<&str>) -> Result<()> {
  let release_arg = release.then_some("--release");
  let target_arg = match target {
    Some(x) => vec!["--target", x],
    None => vec![],
  };
  cmd!(sh, "cargo build {release_arg...} {target_arg...} --locked --bin lang-srv").run()?;
  let mut dir: PathBuf = ["editors", "vscode", "out"].iter().collect();
  sh.remove_path(&dir)?;
  sh.create_dir(&dir)?;
  let kind = if release { "release" } else { "debug" };
  let lang_srv_name = format!("lang-srv{}", std::env::consts::EXE_SUFFIX);
  let lang_srv: PathBuf =
    std::iter::once("target").chain(target).chain([kind, lang_srv_name.as_str()]).collect();
  sh.copy_file(&lang_srv, &dir)?;
  assert!(dir.pop());
  let license_header =
    "Millet is dual-licensed under the terms of both the MIT license and the Apache license v2.0.";
  let license_apache = sh.read_file("license-apache.md")?;
  let license_mit = sh.read_file("license-mit.md")?;
  let license_text = format!("{license_header}\n\n{license_apache}\n{license_mit}");
  dir.push("license.md");
  sh.write_file(&dir, license_text)?;
  assert!(dir.pop());
  let _d = sh.push_dir(&dir);
  if !sh.path_exists("node_modules") {
    if cfg!(windows) {
      cmd!(sh, "cmd.exe /c npm ci").run()?;
    } else {
      cmd!(sh, "npm ci").run()?;
    }
  }
  if cfg!(windows) {
    cmd!(sh, "cmd.exe /c npm run check").run()?;
    cmd!(sh, "cmd.exe /c npm run build-{kind}").run()?;
  } else {
    cmd!(sh, "npm run check").run()?;
    cmd!(sh, "npm run build-{kind}").run()?;
  }
  Ok(())
}

fn run_ci(sh: &Shell) -> Result<()> {
  cmd!(sh, "cargo build --locked").run()?;
  cmd!(sh, "cargo fmt -- --check").run()?;
  cmd!(sh, "cargo clippy").run()?;
  cmd!(sh, "cargo test --locked").run()?;
  ck_sml_def(sh)?;
  ck_test_refs(sh)?;
  ck_no_ignore(sh)?;
  ck_sml_libs(sh)?;
  ck_crate_architecture_doc(sh)?;
  ck_docs_readme(sh)?;
  ck_no_debugging(sh)?;
  // skip on CI because CI may not have tags.
  if option_env!("CI") != Some("1") {
    ck_changelog(sh)?;
  }
  Ok(())
}

fn main() -> Result<()> {
  let mut args = Arguments::from_env();
  let sh = Shell::new()?;
  if args.contains(["-h", "--help"]) {
    show_help();
    return Ok(());
  }
  let cmd: Cmd = match args.subcommand()? {
    Some(x) => x.parse()?,
    None => {
      show_help();
      return Ok(());
    }
  };
  let _d = sh.push_dir(Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap());
  match cmd {
    Cmd::Help => show_help(),
    Cmd::Ci => {
      finish_args(args)?;
      run_ci(&sh)?;
    }
    Cmd::Dist => {
      let release = args.contains("--release");
      let target: Option<String> = args.opt_value_from_str("--target")?;
      finish_args(args)?;
      dist(&sh, release, target.as_deref())?;
    }
    Cmd::Tag => {
      let tag: String = args.free_from_str()?;
      finish_args(args)?;
      let version = match tag.strip_prefix('v') {
        Some(x) => x,
        None => bail!("tag must start with v"),
      };
      let version_parts: Vec<_> = version.split('.').collect();
      let num_parts = version_parts.len();
      if num_parts != 3 {
        bail!("version must have 3 dot-separated parts (got {num_parts})")
      }
      for part in version_parts {
        if let Err(e) = part.parse::<u16>() {
          bail!("{part}: not a non-negative 16-bit integer: {e}")
        }
      }
      let paths: Vec<PathBuf> = ["package.json", "package-lock.json"]
        .into_iter()
        .map(|p| ["editors", "vscode", p].into_iter().collect())
        .collect();
      for path in paths.iter() {
        let contents = std::fs::read_to_string(path)?;
        let mut out = String::with_capacity(contents.len());
        for (idx, line) in contents.lines().enumerate() {
          if idx >= 15 {
            out.push_str(line);
          } else {
            match line.split_once(": ") {
              None => out.push_str(line),
              Some((key, _)) => {
                if key.trim() == "\"version\"" {
                  out.push_str(key);
                  out.push_str(": \"");
                  out.push_str(version);
                  out.push_str("\",");
                } else {
                  out.push_str(line);
                }
              }
            }
          }
          out.push('\n');
        }
        std::fs::write(path, out)?;
      }
      cmd!(sh, "git add {paths...}").run()?;
      let msg = format!("Release {tag}");
      cmd!(sh, "git commit -m {msg} --no-verify").run()?;
      cmd!(sh, "git tag {tag}").run()?;
      run_ci(&sh)?;
    }
  }
  Ok(())
}
