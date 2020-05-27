mod args;

fn run() -> bool {
  let args = args::get();
  let stdout = std::io::stdout();
  let stdout = stdout.lock();
  let mut m = millet_core::source::SourceMap::new();
  let mut w = millet_core::source::Reporter::new(stdout);
  for name in args.files {
    match std::fs::read_to_string(&name) {
      Ok(s) => m.insert(name, s),
      Err(e) => {
        w.report_io(&name, e).unwrap();
        return false;
      }
    }
  }
  for _ in m
    .iter()
    .map(|(id, file)| millet_core::lex::get(id, file.as_bytes()))
  {
    //
  }
  true
}

fn main() {
  if !run() {
    std::process::exit(1);
  }
}
