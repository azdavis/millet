//! A test for the CLI, using real I/O.

use anyhow::{Context, Result, anyhow, bail};
use std::path::Path;
use std::process::Command;
use tempfile::TempDir;

fn root_dir() -> Result<&'static Path> {
  Path::new(env!("CARGO_MANIFEST_DIR")).parent().ok_or_else(|| anyhow!("no parent"))
}

fn exe(s: &str) -> String {
  let suffix = std::env::consts::EXE_SUFFIX;
  format!("{s}{suffix}")
}

pub fn run() -> Result<()> {
  let tmp = TempDir::new().with_context(|| "couldn't make temp dir")?;
  std::fs::create_dir(tmp.path().join("code")).with_context(|| "couldn't make code dir")?;
  let millet_toml = r#"
version = 1
[workspace]
root = "code/sources.mlb"
"#;
  let files: [(&[&str], &str); 3] = [
    (&["millet.toml"], millet_toml),
    (&["code", "sources.mlb"], "test.sml"),
    (&["code", "test.sml"], "val b = true"),
  ];
  for (components, contents) in files {
    let mut path = tmp.path().to_owned();
    path.extend(components);
    std::fs::write(path.as_path(), contents)
      .with_context(|| format!("couldn't write {}", path.display()))?;
  }
  let mut build = Command::new("cargo")
    .args(["build", "--bin", "millet-cli"])
    .spawn()
    .with_context(|| "couldn't spawn millet-cli build")?;
  let wait = build.wait().with_context(|| "couldn't wait for millet-cli build")?;
  if !wait.success() {
    bail!("couldn't build millet-cli successfully");
  }
  let millet_cli_path = root_dir()?.join("target").join("debug").join(exe("millet-cli"));
  let out = Command::new(millet_cli_path)
    .current_dir(tmp.path())
    .arg(".")
    .output()
    .with_context(|| "couldn't run millet-cli with output")?;
  tmp.close().with_context(|| "couldn't remove temp dir")?;
  let stdout = String::from_utf8(out.stdout).with_context(|| "couldn't convert stdout to utf-8")?;
  let stderr = String::from_utf8(out.stderr).with_context(|| "couldn't convert stderr to utf-8")?;
  if !out.status.success() {
    bail!("couldn't run millet-cli successfully");
  }
  if !stderr.is_empty() {
    bail!("didn't get empty stderr for millet-cli: {stderr}");
  }
  if stdout.trim() != "no errors!" {
    bail!("got errors for millet-cli stdout: {stdout}");
  }
  Ok(())
}
