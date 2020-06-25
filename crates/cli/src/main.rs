//! A CLI for millet.

mod args;
mod diagnostic;
mod source;

use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use millet_core::{intern, lex, parse};
use std::io::Write as _;

fn run() -> bool {
  let args = match args::get() {
    Ok(Some(x)) => x,
    Ok(None) => return true,
    Err(e) => {
      eprintln!("{}", e);
      return false;
    }
  };
  let config = term::Config::default();
  let w = StandardStream::stdout(ColorChoice::Auto);
  let mut w = w.lock();
  let mut src = source::SourceMap::new();
  let mut store = intern::StrStoreMut::new();
  for name in args.files {
    match std::fs::read_to_string(&name) {
      Ok(s) => src.insert(name, s),
      Err(e) => {
        term::emit(&mut w, &config, &src, &diagnostic::io(&name, e)).expect("io error");
        return false;
      }
    }
  }
  let mut lexers = Vec::with_capacity(src.len());
  for (id, file) in src.iter() {
    match lex::get(&mut store, file.as_bytes()) {
      Ok(lexer) => lexers.push(lexer),
      Err(e) => {
        term::emit(&mut w, &config, &src, &diagnostic::lex(id, e)).expect("io error");
        return false;
      }
    }
  }
  let store = store.finish();
  for ((id, file), lexer) in src.iter().zip(lexers) {
    match parse::get(lexer) {
      Ok(xs) => {
        if args.just_ast {
          writeln!(w, "{}: {:#?}", file.name(), xs).expect("io error");
        }
      }
      Err(e) => {
        term::emit(&mut w, &config, &src, &diagnostic::parse(&store, id, e)).expect("io error");
        return false;
      }
    }
  }
  if args.just_ast {
    return true;
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
