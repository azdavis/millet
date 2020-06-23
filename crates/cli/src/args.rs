//! Command-line arguments.

pub fn get() -> Result<Option<Args>, pico_args::Error> {
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
    show_ast: args.contains("--show-ast"),
    files: args.free()?,
  }))
}

pub struct Args {
  pub show_ast: bool,
  pub files: Vec<String>,
}
