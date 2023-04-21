//! A thin CLI front-end for running Millet once over some files.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use paths::FileSystem as _;

fn usage() {
  let current_exe_name = std::env::current_exe()
    .ok()
    .and_then(|x| Some(x.file_name()?.to_str()?.to_owned()))
    .unwrap_or_else(|| "<unknown>".to_owned());
  println!("usage:");
  println!("  {current_exe_name} [options] <path>");
  let rest_of_usage = r#"
options:
  -h, --help
    show this help

arguments:
  <path>
    path of the project to analyze. the path is a directory containing either:
    - a single .cm or .mlb file
    - a millet.toml config file
"#;
  print!("{rest_of_usage}");
}

fn run() -> usize {
  match env_logger::try_init_from_env(env_logger::Env::default().default_filter_or("error")) {
    Ok(()) => {}
    Err(e) => {
      println!("could not start env logger: {e}");
      return 1;
    }
  }
  let mut args = pico_args::Arguments::from_env();
  if args.contains(["-h", "--help"]) {
    usage();
    return 0;
  }
  let root: std::path::PathBuf = match args.free_from_str() {
    Ok(x) => x,
    Err(e) => {
      println!("error[{}]: {}", diagnostic::Code::n(1019), e);
      return 1;
    }
  };
  let fs = paths::RealFileSystem::default();
  let root = match fs.canonicalize(root.as_path()) {
    Ok(x) => x,
    Err(e) => {
      show_input_error(root.as_path(), &input::Error::from_io(root.clone(), e));
      return 1;
    }
  };
  let mut store = paths::Store::new();
  let inp = input::Input::new(&fs, &mut store, &root);
  let mut an = analysis::Analysis::new(
    analysis::StdBasis::full(),
    config::ErrorLines::One,
    Some(config::init::DiagnosticsIgnore::AfterSyntax),
    None,
  );
  let got = an.get_many(&inp);
  for err in &inp.errors {
    show_input_error(root.as_path(), err);
  }
  for (&path, ds) in &got {
    let path = store.get_path(path);
    for d in ds {
      show_diagnostic(root.as_path(), path.as_path(), d);
    }
  }
  inp.errors.len() + got.values().map(Vec::len).sum::<usize>()
}

fn show_input_error(root: &std::path::Path, e: &input::Error) {
  print!("{}", e.maybe_rel_path(root).display());
  if let Some(r) = e.range() {
    print!(":{}", r.start);
  }
  let code = e.code();
  println!(": error[{code}]: {}", e.display(root));
}

fn show_diagnostic<R>(root: &std::path::Path, path: &std::path::Path, d: &analysis::Diagnostic<R>)
where
  R: std::fmt::Display,
{
  let path = path.strip_prefix(root).unwrap_or(path);
  println!("{}:{}: {}[{}]: {}", path.display(), d.range, d.severity, d.code, d.message);
}

fn main() {
  panic_hook::install();
  match run() {
    0 => println!("no errors!"),
    n => {
      let suffix = if n == 1 { "" } else { "s" };
      println!("{n} error{suffix}. see {} for more information", analysis::URL);
      std::process::exit(1)
    }
  }
}
