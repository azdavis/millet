//! A CLI for millet.

mod args;
mod source;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use millet_core::{intern, lex, parse, statics};
use std::io::Write as _;

fn simple<M, T, R>(msg: M, id: T, loc: R) -> Diagnostic<T>
where
  M: Into<String>,
  R: Into<std::ops::Range<usize>>,
{
  Diagnostic::error()
    .with_message(msg)
    .with_labels(vec![Label::primary(id, loc)])
}

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
        let diag = Diagnostic::error().with_message(format!("{}: {}", name, e));
        term::emit(&mut w, &config, &src, &diag).unwrap();
        writeln!(&mut w, "file i/o failed").unwrap();
        return false;
      }
    }
  }
  let mut lexers = Vec::with_capacity(src.len());
  for (id, file) in src.iter() {
    match lex::get(&mut store, file.as_bytes()) {
      Ok(lexer) => lexers.push(lexer),
      Err(e) => {
        let diag = simple(e.val.message(), id, e.loc);
        term::emit(&mut w, &config, &src, &diag).unwrap();
        writeln!(&mut w, "lexing failed").unwrap();
        return false;
      }
    }
  }
  let store = store.finish();
  let mut top_decs = Vec::with_capacity(src.len());
  for ((id, file), lexer) in src.iter().zip(lexers) {
    match parse::get(lexer) {
      Ok(xs) => {
        if args.just_ast {
          writeln!(w, "{}: {:#?}", file.name(), xs).unwrap();
        } else {
          top_decs.push((id, xs));
        }
      }
      Err(e) => {
        let diag = simple(e.val.message(&store), id, e.loc);
        term::emit(&mut w, &config, &src, &diag).unwrap();
        writeln!(&mut w, "parsing failed").unwrap();
        return false;
      }
    }
  }
  if args.just_ast {
    return true;
  }
  let mut s = statics::Statics::new();
  for (id, xs) in top_decs {
    for x in xs {
      match s.get(&x) {
        Ok(()) => {}
        Err(e) => {
          let diag = simple(e.val.message(&store), id, e.loc);
          term::emit(&mut w, &config, &src, &diag).unwrap();
          writeln!(&mut w, "typechecking failed").unwrap();
          return false;
        }
      }
    }
  }
  s.finish();
  if !args.quiet {
    writeln!(&mut w, "no errors").unwrap();
  }
  true
}

fn main() {
  match std::thread::Builder::new()
    .name("run".to_owned())
    .stack_size(10 * 1024 * 1024)
    .spawn(run)
    .unwrap()
    .join()
  {
    Err(_) | Ok(false) => std::process::exit(1),
    Ok(true) => {}
  }
}
