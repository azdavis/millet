//! A CLI for millet.

mod args;
mod diagnostic;
mod source;

use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use millet_core::{lex, parse};
use std::io::Write as _;

fn run() -> bool {
  let args = args::get();
  let config = term::Config::default();
  let w = StandardStream::stdout(ColorChoice::Auto);
  let mut w = w.lock();
  let mut m = source::SourceMap::new();
  for name in args.files {
    match std::fs::read_to_string(&name) {
      Ok(s) => m.insert(name, s),
      Err(e) => {
        writeln!(w, "io error: {}: {}", name, e).unwrap();
        return false;
      }
    }
  }
  for (id, file) in m.iter() {
    match parse::get(lex::get(file.as_bytes())) {
      Ok(xs) => eprintln!("parsed: {:#?}", xs),
      Err(e) => {
        term::emit(&mut w, &config, &m, &diagnostic::new(id, e)).unwrap();
        return false;
      }
    }
  }
  true
}

fn main() {
  if !run() {
    std::process::exit(1);
  }
  println!("OK");
}
