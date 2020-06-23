//! A CLI for millet.

mod args;
mod diagnostic;
mod source;

use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use millet_core::{error, intern, lex, parse};
use std::io::Write as _;

fn run() -> bool {
  let args = args::get();
  if args.version {
    println!("{}", env!("CARGO_PKG_VERSION"));
    return true;
  }
  let config = term::Config::default();
  let w = StandardStream::stdout(ColorChoice::Auto);
  let mut w = w.lock();
  let mut src = source::SourceMap::new();
  let mut store = intern::StrStoreMut::new();
  for name in args.files {
    match std::fs::read_to_string(&name) {
      Ok(s) => src.insert(name, s),
      Err(e) => {
        writeln!(w, "io error: {}: {}", name, e).expect("couldn't output");
        return false;
      }
    }
  }
  let mut lexers = Vec::with_capacity(src.len());
  for (id, file) in src.iter() {
    match lex::get(&mut store, file.as_bytes()) {
      Ok(lexer) => lexers.push(lexer),
      Err(e) => {
        let diag = diagnostic::new(&store.finish(), id, error::Error::Lex(e));
        term::emit(&mut w, &config, &src, &diag).expect("couldn't output");
        return false;
      }
    }
  }
  let store = store.finish();
  for ((id, file), lexer) in src.iter().zip(lexers) {
    match parse::get(lexer) {
      Ok(xs) => {
        if args.show_ast {
          writeln!(w, "{}: {:#?}", file.name(), xs).expect("couldn't output")
        }
      }
      Err(e) => {
        let diag = diagnostic::new(&store, id, error::Error::Parse(e));
        term::emit(&mut w, &config, &src, &diag).expect("couldn't output");
        return false;
      }
    }
  }
  true
}

fn main() {
  match std::thread::Builder::new()
    .name("run".to_owned())
    .stack_size(10 * 1024 * 1024)
    .spawn(run)
    .expect("couldn't spawn run")
    .join()
  {
    Err(_) | Ok(false) => std::process::exit(1),
    Ok(true) => {}
  }
}
