//! See https://github.com/matklad/cargo-xtask.

use anyhow::{bail, Result};
use fast_hash::FxHashSet;
use pico_args::Arguments;
use std::path::Path;
use xshell::{cmd, Shell};

enum Cmd {
  Help,
  Test,
  CkSmlDef,
  MkVscExt,
}

impl Cmd {
  const VALUES: [Cmd; 4] = [Cmd::Help, Cmd::Test, Cmd::CkSmlDef, Cmd::MkVscExt];

  fn name_desc(&self) -> (&'static str, &'static str) {
    match self {
      Cmd::Help => ("help", "show this help"),
      Cmd::Test => ("test", "run various tests"),
      Cmd::CkSmlDef => (
        "ck-sml-def",
        "check whether the sml definition is properly referenced",
      ),
      Cmd::MkVscExt => ("mk-vsc-ext", "make a vscode extension"),
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
  let out = cmd!(
    sh,
    "git grep -hoE 'SML Definition \\(([[:digit:]]+)\\)' crates/old-statics/src"
  )
  .output()?;
  let got: FxHashSet<u16> = String::from_utf8(out.stdout)?
    .lines()
    .filter_map(|line| {
      let (_, inner) = line.split_once('(')?;
      let (num, _) = inner.split_once(')')?;
      num.parse().ok()
    })
    .collect();
  let missing: Vec<_> = (1u16..=89).filter(|x| !got.contains(x)).collect();
  if !missing.is_empty() {
    bail!("missing sml definition references: {missing:?}")
  }
  Ok(())
}

fn mk_vsc(sh: &Shell) -> Result<()> {
  cmd!(sh, "cargo build --bin lang-srv").run()?;
  let out = "extensions/vscode/out";
  sh.remove_path(out)?;
  sh.create_dir(out)?;
  sh.copy_file("target/debug/lang-srv", out)?;
  let _d = sh.push_dir("extensions/vscode");
  if !Path::new("node_modules").exists() {
    cmd!(sh, "npm ci").run()?;
  }
  cmd!(sh, "npm run build").run()?;
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
    Cmd::Test => {
      finish_args(args)?;
      cmd!(sh, "cargo build --locked").run()?;
      cmd!(sh, "cargo fmt -- --check").run()?;
      cmd!(sh, "cargo clippy").run()?;
      cmd!(sh, "cargo test --locked").run()?;
      ck_sml_def(&sh)?;
    }
    Cmd::CkSmlDef => {
      finish_args(args)?;
      ck_sml_def(&sh)?;
    }
    Cmd::MkVscExt => {
      finish_args(args)?;
      mk_vsc(&sh)?;
    }
  }
  Ok(())
}
