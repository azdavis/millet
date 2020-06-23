//! Command-line arguments.

use gumdrop::Options;

/// Returns the command-line arguments parsed into an `Args` struct, or exit the process if the
/// command-line arguments were invalid or if the help flag was passed.
pub fn get() -> Args {
  Args::parse_args_default_or_exit()
}

#[derive(Debug, Options)]
pub struct Args {
  #[options(free, help = "Source files")]
  pub files: Vec<String>,
  /// Always `false` since `get` will exit the process if the help flag is detected.
  #[options(help = "Show this help")]
  pub help: bool,
  #[options(help = "Show the version")]
  pub version: bool,
  #[options(help = "Show AST")]
  pub show_ast: bool,
}
