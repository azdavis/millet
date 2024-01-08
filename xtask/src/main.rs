//! See the [xtask spec](https://github.com/matklad/cargo-xtask).

#![allow(clippy::single_match_else)]

mod cli_test;

use anyhow::{anyhow, bail, Context as _, Result};
use flate2::{write::GzEncoder, Compression};
use pico_args::Arguments;
use std::path::{Path, PathBuf};
use std::{env, fs, io, process::Command};

#[derive(Debug, Clone, Copy)]
enum Cmd {
  Help,
  Ci,
  CliTest,
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
  const VALUES: [Cmd; 5] = [Cmd::Help, Cmd::Ci, Cmd::CliTest, Cmd::Dist, Cmd::Tag];

  fn spec(self) -> CmdSpec {
    match self {
      Cmd::Help => CmdSpec { name: "help", desc: "show this help", options: &[], args: &[] },
      Cmd::Ci => CmdSpec {
        name: "ci",
        desc: "run various tests, including the cli test",
        options: &[],
        args: &[],
      },
      Cmd::CliTest => CmdSpec { name: "cli-test", desc: "run a cli test", options: &[], args: &[] },
      Cmd::Dist => CmdSpec {
        name: "dist",
        desc: "make artifacts for distribution",
        options: &[
          ("--release", "build for release"),
          (
            "--target <target>",
            "build for the <target> triple, and gzip, and build the CLI as well",
          ),
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

fn run(c: &mut Command) -> Result<()> {
  let mut sp = c.spawn().with_context(|| format!("spawn {c:?}"))?;
  let w = sp.wait().with_context(|| format!("wait for {c:?}"))?;
  if w.success() {
    Ok(())
  } else {
    bail!("unsuccessful {c:?}")
  }
}

fn run_ci() -> Result<()> {
  run(Command::new("cargo").args(["build", "--locked"]))?;
  run(Command::new("cargo").args(["fmt", "--", "--check"]))?;
  run(Command::new("cargo").args(["clippy", "--locked"]))?;
  run(Command::new("cargo").args(["test", "--locked"]))?;
  if env_var_enabled("SKIP_FULL_STD_BASIS") {
    println!("note: SKIP_FULL_STD_BASIS env var was set to 1");
    println!("note: this means some slower tests were skipped (but appear as passed)");
  }
  cli_test::run()?;
  Ok(())
}

fn env_var_enabled(s: &str) -> bool {
  env::var_os(s).map_or(false, |x| x == "1")
}

#[derive(Debug)]
struct DistArgs {
  release: bool,
  target: Option<String>,
}

const LANG_SRV_NAME: &str = "millet-ls";
const CLI_NAME: &str = "millet-cli";

fn cmd_exe(fst: &str) -> Command {
  if cfg!(windows) {
    let mut ret = Command::new("cmd.exe");
    ret.arg("/c").arg(fst);
    ret
  } else {
    Command::new(fst)
  }
}

fn pop_path_buf(p: &mut PathBuf) -> Result<()> {
  if p.pop() {
    Ok(())
  } else {
    bail!("path had no parent")
  }
}

fn exe(s: &str) -> String {
  let suffix = env::consts::EXE_SUFFIX;
  format!("{s}{suffix}")
}

fn dist(args: &DistArgs) -> Result<()> {
  let mut c = Command::new("cargo");
  c.args(["build", "--locked", "--bin", LANG_SRV_NAME]);
  if args.release {
    c.arg("--release");
  }
  if let Some(target) = &args.target {
    c.args(["--bin", CLI_NAME, "--target", target.as_str()]);
  }
  run(&mut c)?;
  let kind = if args.release { "release" } else { "debug" };
  let mut src: PathBuf =
    std::iter::once("target").chain(args.target.as_deref()).chain(std::iter::once(kind)).collect();
  let mut dst: PathBuf;
  if let Some(target) = &args.target {
    dst = PathBuf::from("binary");
    fs::create_dir_all(&dst).with_context(|| format!("create dir {}", dst.display()))?;
    for name in [LANG_SRV_NAME, CLI_NAME] {
      let gz = format!("{name}-{target}.gz");
      src.push(exe(name).as_str());
      dst.push(gz.as_str());
      gzip(&src, &dst)?;
      pop_path_buf(&mut src)?;
      pop_path_buf(&mut dst)?;
    }
  }
  dst = ["editors", "vscode", "out"].into_iter().collect();
  // ignore errors if it exists already. if we have permission errors we're about to report them
  // with the create_dir_all anyway
  _ = fs::remove_dir_all(&dst);
  fs::create_dir_all(&dst).with_context(|| format!("create dir {}", dst.display()))?;
  src.push(exe(LANG_SRV_NAME).as_str());
  dst.push(exe(LANG_SRV_NAME).as_str());
  fs::copy(&src, &dst).with_context(|| format!("copy {} to {}", src.display(), dst.display()))?;
  pop_path_buf(&mut dst)?;
  pop_path_buf(&mut dst)?;
  let license_text = {
    let header =
      "Millet is licensed under either the MIT license or the Apache license v2.0, at your option.";
    let apache = include_str!("../../LICENSE-APACHE.md");
    let mit = include_str!("../../LICENSE-MIT.md");
    format!("{header}\n\n{apache}\n{mit}")
  };
  dst.push("CHANGELOG.md");
  let changelog = include_str!("../../docs/CHANGELOG.md");
  fs::write(&dst, changelog)?;
  pop_path_buf(&mut dst)?;
  dst.push("LICENSE.md");
  fs::write(&dst, license_text).with_context(|| format!("write {}", dst.display()))?;
  pop_path_buf(&mut dst)?;
  env::set_current_dir(&dst).with_context(|| format!("set current dir to {}", dst.display()))?;
  if fs::metadata("node_modules").is_err() {
    run(cmd_exe("npm").arg("ci"))?;
  }
  run(cmd_exe("npm").args(["run", "check"]))?;
  run(cmd_exe("npm").args(["run", format!("build-{kind}").as_str()]))?;
  Ok(())
}

fn gzip(src: &Path, dst: &Path) -> anyhow::Result<()> {
  let dst_file = fs::File::create(dst).with_context(|| format!("create {}", dst.display()))?;
  let mut encoder = GzEncoder::new(dst_file, Compression::best());
  let src_file = fs::File::open(src).with_context(|| format!("open {}", src.display()))?;
  let mut input = io::BufReader::new(src_file);
  io::copy(&mut input, &mut encoder)
    .with_context(|| format!("gzip {} to {}", src.display(), dst.display()))?;
  encoder.finish().with_context(|| "finish gzip encoding")?;
  Ok(())
}

fn modify_each_line<P, F>(path: P, mut f: F) -> Result<()>
where
  P: AsRef<Path>,
  F: FnMut(&mut String, usize, &str),
{
  let contents = fs::read_to_string(path.as_ref())
    .with_context(|| format!("read {}", path.as_ref().display()))?;
  let mut out = String::with_capacity(contents.len());
  for (idx, line) in contents.lines().enumerate() {
    f(&mut out, idx, line);
    out.push('\n');
  }
  fs::write(path.as_ref(), out).with_context(|| format!("write {}", path.as_ref().display()))
}

fn tag(tag_arg: &str) -> Result<()> {
  let Some(version) = tag_arg.strip_prefix('v') else { bail!("tag must start with v") };
  let version_parts: Vec<_> = version.split('.').collect();
  let num_parts = version_parts.len();
  if num_parts != 3 {
    bail!("version must have 3 dot-separated parts (got {num_parts})");
  }
  for part in version_parts {
    if let Err(e) = part.parse::<u16>() {
      bail!("{part}: not a non-negative 16-bit integer: {e}");
    }
  }
  let paths: Vec<PathBuf> = ["package.json", "package-lock.json"]
    .into_iter()
    .map(|p| ["editors", "vscode", p].into_iter().collect())
    .collect();
  for path in &paths {
    modify_each_line(path, |out, idx, line| {
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
    })?;
  }
  let cargo_toml = "Cargo.toml";
  modify_each_line(cargo_toml, |out, _, line| {
    match line.strip_prefix("version = \"").and_then(|x| x.strip_suffix('"')) {
      None => out.push_str(line),
      Some(_) => {
        out.push_str("version = ");
        out.push('"');
        out.push_str(version);
        out.push('"');
      }
    }
  })?;
  // to update Cargo.lock
  run(Command::new("cargo").arg("build"))?;
  run(Command::new("git").arg("add").args(paths).args([
    "Cargo.toml",
    "Cargo.lock",
    "docs/CHANGELOG.md",
  ]))?;
  let msg = format!("Release {tag_arg}");
  run(Command::new("git").args(["commit", "-m", msg.as_str(), "--no-verify"]))?;
  run(Command::new("git").arg("tag").arg(tag_arg))?;
  Ok(())
}

fn main() -> Result<()> {
  let mut args = Arguments::from_env();
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
  env::set_current_dir(Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap())?;
  match cmd {
    Cmd::Help => show_help(),
    Cmd::Ci => {
      finish_args(args)?;
      run_ci()?;
    }
    Cmd::CliTest => {
      finish_args(args)?;
      cli_test::run()?;
    }
    Cmd::Dist => {
      let dist_args = DistArgs {
        release: args.contains("--release"),
        target: args.opt_value_from_str("--target")?,
      };
      finish_args(args)?;
      dist(&dist_args)?;
    }
    Cmd::Tag => {
      let tag_arg: String = args.free_from_str()?;
      finish_args(args)?;
      tag(&tag_arg)?;
      run_ci()?;
    }
  }
  Ok(())
}
