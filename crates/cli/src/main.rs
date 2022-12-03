//! A thin CLI front-end for running Millet once over some files.

use paths::FileSystem as _;

fn usage() {
  let current_exe_name = std::env::current_exe()
    .ok()
    .and_then(|x| x.file_name().map(|x| x.to_string_lossy().into_owned()))
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
    - a millet.toml config file specifying a single .cm or .mlb file
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
  let root: String = match args.free_from_str() {
    Ok(x) => x,
    Err(e) => {
      println!("error[{}]: {}", diagnostic_util::Code::n(1019), e);
      return 1;
    }
  };
  let fs = paths::RealFileSystem::default();
  let root = std::path::Path::new(root.as_str());
  let root = match fs.canonicalize(root) {
    Ok(x) => x,
    Err(e) => {
      handle_input_error(root, analysis::input::Error::from_io(root.to_owned(), e));
      return 1;
    }
  };
  let mut store = paths::Store::new();
  let inp = match analysis::input::Input::new(&fs, &mut store, &root) {
    Ok(x) => x,
    Err(e) => {
      handle_input_error(root.as_path(), e);
      return 1;
    }
  };
  let mut an = analysis::Analysis::new(
    analysis::StdBasis::Full,
    config::ErrorLines::One,
    config::DiagnosticsFilter::Syntax,
    false,
  );
  let got = an.get_many(&inp);
  let num_errors: usize = got.iter().map(|(_, es)| es.len()).sum();
  for (path, errors) in got {
    for e in errors {
      let path = store.get_path(path);
      let path = path.as_path().strip_prefix(root.as_path()).unwrap_or(path.as_path());
      println!("{}:{}: error[{}]: {}", path.display(), e.range.start, e.code, e.message);
    }
  }
  num_errors
}

fn handle_input_error(root: &std::path::Path, e: analysis::input::Error) {
  print!("{}", e.maybe_rel_path(root).display());
  if let Some(r) = e.range() {
    print!(":{}", r.start);
  }
  let code = e.code();
  println!(": error[{code}]: {}", e.display(root));
}

fn main() {
  match run() {
    0 => println!("no errors"),
    n => {
      let suffix = if n == 1 { "" } else { "s" };
      println!("{n} error{suffix}. see {} for more information", diagnostic_util::URL);
      std::process::exit(1)
    }
  }
}
