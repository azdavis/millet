//! Command-line arguments.

use gumdrop::Options;

pub fn get() -> Args {
  Args::parse_args_default_or_exit()
}

#[derive(Options)]
pub struct Args {
  /// Always `false` since `get` exits the process when `--help` is passed.
  #[options(help = "Show this help")]
  help: bool,
  #[options(help = "Show the version")]
  pub version: bool,
  #[options(help = "Show AST")]
  pub show_ast: bool,
  #[options(free, help = "Source files")]
  pub files: Vec<String>,
}
