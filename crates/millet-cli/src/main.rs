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
  --unsafe-format
    WARNING: THE FORMATTER IS HIGHLY EXPERIMENTAL.
    IT MAY IRREVOCABLY DESTROY SOME OR ALL OF YOUR CODE.
    format all of the SML files

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
  let format = args.contains("--unsafe-format");
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
    format.then_some(config::init::FormatEngine::Naive),
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
  let mut format_errors = 0usize;
  if format {
    for &id in inp.sources.keys() {
      let path = store.get_path(id);
      match an.format(id, 2) {
        Ok((contents, _)) => match std::fs::write(path.as_path(), contents.as_str()) {
          Ok(()) => {}
          Err(e) => {
            let e = input::Error::from_io(path.as_path().to_owned(), e);
            show_input_error(path.as_path(), &e);
            format_errors += 1;
          }
        },
        Err(e) => match e {
          analysis::FormatError::NaiveFmt(e) => match e {
            // should have already reported an error
            sml_naive_fmt::Error::Syntax => {}
            sml_naive_fmt::Error::Comments(ranges) => {
              for range in ranges {
                let range = an.source_range_utf16(id, range).expect("no range");
                let d = analysis::Diagnostic::naive_fmt_comment(range);
                show_diagnostic(root.as_path(), path.as_path(), &d);
                format_errors += 1;
              }
            }
          },
          analysis::FormatError::Disabled => {
            unreachable!("we enabled formatting if `format` is true")
          }
          analysis::FormatError::NoFile => {
            unreachable!("formatting a file from `inp` should exist")
          }
          analysis::FormatError::Smlfmt(_) => unreachable!("we're not using `smlfmt`"),
        },
      }
    }
  }
  format_errors + inp.errors.len() + got.values().map(Vec::len).sum::<usize>()
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
