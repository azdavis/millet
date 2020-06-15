//! Command-line arguments.

use clap::{App, Arg, ArgMatches};

pub fn get() -> Args {
  get_impl(app().get_matches())
}

#[derive(Debug)]
pub struct Args {
  pub files: Vec<String>,
  pub show_ast: bool,
}

fn app() -> App<'static, 'static> {
  App::new("millet")
    .version(clap::crate_version!())
    .about("An implementation of Standard ML")
    .arg(
      Arg::with_name("show-ast")
        .help("Just show AST")
        .long("show-ast"),
    )
    .arg(Arg::with_name("file").help("Source file").multiple(true))
}

fn get_impl(matches: ArgMatches<'static>) -> Args {
  Args {
    files: match matches.values_of("file") {
      None => vec![],
      Some(fs) => fs.map(ToOwned::to_owned).collect(),
    },
    show_ast: matches.is_present("show-ast"),
  }
}
