mod args;

use millet_core::lex;
use millet_core::source::{Reporter, SourceMap};

fn run() -> bool {
  let args = args::get();
  let stdout = std::io::stdout();
  let mut m = SourceMap::new();
  let mut w = Reporter::new(stdout.lock());
  for name in args.files {
    match std::fs::read_to_string(&name) {
      Ok(s) => m.insert(name, s),
      Err(e) => {
        w.report_io(&name, e).unwrap();
        return false;
      }
    }
  }
  for _ in m.iter().map(|(id, file)| lex::get(id, file.as_bytes())) {
    //
  }
  true
}

fn main() {
  if !run() {
    std::process::exit(1);
  }
}
