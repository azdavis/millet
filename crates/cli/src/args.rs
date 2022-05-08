//! Command-line arguments.

use std::ffi::OsString;

pub fn get() -> Result<Option<Args>, OsString> {
  let mut args = pico_args::Arguments::from_env();
  if args.contains(["-h", "--help"]) {
    print!("{}", include_str!("help.txt"));
    return Ok(None);
  }
  if args.contains(["-v", "--version"]) {
    println!("{}", env!("CARGO_PKG_VERSION"));
    return Ok(None);
  }
  Ok(Some(Args {
    quiet: args.contains(["-q", "--quiet"]),
    just_ast: args.contains("--just-ast"),
    files: args
      .finish()
      .into_iter()
      .map(|x| x.into_string())
      .collect::<Result<Vec<_>, _>>()?,
  }))
}

pub struct Args {
  pub quiet: bool,
  pub just_ast: bool,
  pub files: Vec<String>,
}
