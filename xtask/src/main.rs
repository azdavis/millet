//! See the [xtask spec](https://github.com/matklad/cargo-xtask).

use anyhow::{anyhow, bail, Result};
use pico_args::Arguments;
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
  let license_apache = sh.read_file("LICENSE-APACHE.md")?;
  let license_mit = sh.read_file("LICENSE-MIT.md")?;
  let license_text = format!("{license_header}\n\n{license_apache}\n{license_mit}");
  dir.push("LICENSE.md");
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
        let contents = sh.read_file(path)?;
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
        sh.write_file(path, out)?;
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
