//! See the [xtask spec](https://github.com/matklad/cargo-xtask).

use anyhow::{bail, Result};
use pico_args::Arguments;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use xshell::{cmd, Shell};

enum Cmd {
  Help,
  Ci,
  Dist,
  Tag,
}

impl Cmd {
  const VALUES: [Cmd; 4] = [Cmd::Help, Cmd::Ci, Cmd::Dist, Cmd::Tag];

  fn name_desc(&self) -> (&'static str, &'static str) {
    match self {
      Cmd::Help => ("help", "show this help"),
      Cmd::Ci => ("ci", "run various tests"),
      Cmd::Dist => (
        "dist",
        "make artifacts for distribution (can use --release)",
      ),
      Cmd::Tag => (
        "tag",
        "update package files, then commit and tag a new release",
      ),
    }
  }
}

impl std::str::FromStr for Cmd {
  type Err = anyhow::Error;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    for c in Cmd::VALUES {
      let (name, _) = c.name_desc();
      if name == s {
        return Ok(c);
      }
    }
    bail!("couldn't parse {s} into a command")
  }
}

fn show_help() {
  println!("usage:");
  println!("  cargo xtask <command>");
  println!();
  println!("commands:");
  for c in Cmd::VALUES {
    let (name, desc) = c.name_desc();
    println!("  {name}");
    println!("    {desc}");
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
  println!("checking sml definition");
  let dirs: [PathBuf; 3] =
    ["hir", "lower", "statics"].map(|x| ["crates", x, "src"].iter().collect());
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

fn ck_no_ignore(sh: &Shell) -> Result<()> {
  println!("checking for no ignores");
  let has_ignore = cmd!(sh, "git grep -lFe '#[ignore'").output()?;
  let out = String::from_utf8(has_ignore.stdout)?;
  let set: BTreeSet<_> = out.lines().filter(|&x| x != "xtask/src/main.rs").collect();
  if set.is_empty() {
    Ok(())
  } else {
    bail!("found files with ignore: {set:?}");
  }
}

fn ck_std_basis(sh: &Shell) -> Result<()> {
  println!("checking std basis files");
  let order_file = sh.read_file("crates/analysis/src/std_basis/mod.rs")?;
  let mut order: Vec<_> = order_file
    .lines()
    .filter_map(|x| x.strip_prefix("  include_str!(\"")?.strip_suffix("\"),"))
    .collect();
  order.sort_unstable();
  let std_basis_dir = sh.read_dir("crates/analysis/src/std_basis")?;
  let files: Vec<_> = std_basis_dir
    .iter()
    .filter_map(|x| {
      if x.extension()? == "sml" {
        x.file_name()?.to_str()
      } else {
        None
      }
    })
    .collect();
  if files != order {
    bail!("bad order of std_basis files\n  expected {files:?}\n     found {order:?}")
  }
  Ok(())
}

fn dist(sh: &Shell, release: bool) -> Result<()> {
  let release_arg = release.then(|| "--release");
  cmd!(sh, "cargo build {release_arg...} --locked --bin lang-srv").run()?;
  let mut dir: PathBuf = ["extensions", "vscode", "out"].iter().collect();
  sh.remove_path(&dir)?;
  sh.create_dir(&dir)?;
  let kind = if release { "release" } else { "debug" };
  let lang_srv_name = format!("lang-srv{}", std::env::consts::EXE_SUFFIX);
  let lang_srv: PathBuf = ["target", kind, lang_srv_name.as_str()].iter().collect();
  sh.copy_file(&lang_srv, &dir)?;
  assert!(dir.pop());
  sh.copy_file("license.md", &dir)?;
  let _d = sh.push_dir(&dir);
  if !sh.path_exists("node_modules") {
    if cfg!(windows) {
      cmd!(sh, "cmd.exe /c npm ci").run()?;
    } else {
      cmd!(sh, "npm ci").run()?;
    }
  }
  if cfg!(windows) {
    cmd!(sh, "cmd.exe /c npm run build-{kind}").run()?;
  } else {
    cmd!(sh, "npm run build-{kind}").run()?;
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
      cmd!(sh, "cargo build --locked").run()?;
      cmd!(sh, "cargo fmt -- --check").run()?;
      cmd!(sh, "cargo clippy").run()?;
      cmd!(sh, "cargo test --locked").run()?;
      ck_sml_def(&sh)?;
      ck_no_ignore(&sh)?;
      ck_std_basis(&sh)?;
    }
    Cmd::Dist => {
      let release = args.contains("--release");
      finish_args(args)?;
      dist(&sh, release)?;
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
        .map(|p| ["extensions", "vscode", p].into_iter().collect())
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
      let release_msg = format!("Release {tag}");
      cmd!(sh, "git commit -m {release_msg}").run()?;
      cmd!(sh, "git tag {tag}").run()?;
    }
  }
  Ok(())
}
