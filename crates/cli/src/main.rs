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
  let config = term::Config::default();
  let writer = StandardStream::stdout(ColorChoice::Auto);
  let mut writer = writer.lock();
  let mut source_map = source::SourceMap::new();
  let mut store = intern::StrStoreMut::new();
  for name in args.files {
    match std::fs::read_to_string(&name) {
      Ok(s) => source_map.insert(name, s),
      Err(e) => {
        writeln!(writer, "io error: {}: {}", name, e).unwrap();
        return false;
      }
    }
  }
  let mut lexers = Vec::with_capacity(source_map.len());
  for (id, file) in source_map.iter() {
    match lex::get(&mut store, file.as_bytes()) {
      Ok(lexer) => lexers.push(lexer),
      Err(e) => {
        term::emit(
          &mut writer,
          &config,
          &source_map,
          &diagnostic::new(&store.finish(), id, error::Error::Lex(e)),
        )
        .unwrap();
        return false;
      }
    }
  }
  let store = store.finish();
  for ((id, _), lexer) in source_map.iter().zip(lexers) {
    match parse::get(lexer) {
      Ok(xs) => eprintln!("parsed: {:#?}", xs),
      Err(e) => {
        term::emit(
          &mut writer,
          &config,
          &source_map,
          &diagnostic::new(&store, id, error::Error::Parse(e)),
        )
        .unwrap();
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
