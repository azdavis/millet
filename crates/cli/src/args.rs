//! Command-line arguments.

use gumdrop::Options;

pub fn get() -> Args {
  Args::parse_args_default_or_exit()
}

#[derive(Debug, Options)]
pub struct Args {
  #[options(free, help = "Source file(s)")]
  pub files: Vec<String>,
  #[options(help = "Just show AST")]
  pub show_ast: bool,
  #[options(help = "Show this help")]
  pub help: bool,
}
