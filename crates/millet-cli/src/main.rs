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
  let options = analysis::Options {
    lines: config::DiagnosticLines::One,
    ignore: config::init::DiagnosticsIgnore::AfterSyntax,
    format: if format {
      config::init::FormatEngine::Naive
    } else {
      config::init::FormatEngine::None
    },
  };
  let mut an = analysis::Analysis::new(analysis::StdBasis::full(), options);
  let got = an.get_many_text_range(&inp);
  for err in &inp.errors {
    show_input_error(root.as_path(), err);
  }
  let mut stderr = codespan_reporting::term::termcolor::StandardStream::stderr(
    codespan_reporting::term::termcolor::ColorChoice::Auto,
  );
  let config = codespan_reporting::term::Config::default();
  let files = Files { store: &store, input: &inp, analysis: &an };
  for (&path, ds) in &got {
    for d in ds {
      let d = mk_diagnostic(path, d);
      codespan_reporting::term::emit(&mut stderr, &config, &files, &d).unwrap();
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
                let d = analysis::Diagnostic::naive_fmt_comment(range);
                let d = mk_diagnostic(id, &d);
                codespan_reporting::term::emit(&mut stderr, &config, &files, &d).unwrap();
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

fn mk_diagnostic<R>(
  path: paths::PathId,
  d: &analysis::Diagnostic<R>,
) -> codespan_reporting::diagnostic::Diagnostic<paths::PathId>
where
  R: Copy + Into<std::ops::Range<usize>>,
{
  let sev = match d.severity {
    diagnostic::Severity::Warning => codespan_reporting::diagnostic::Severity::Warning,
    diagnostic::Severity::Error => codespan_reporting::diagnostic::Severity::Error,
  };
  let lab = codespan_reporting::diagnostic::Label::new(
    codespan_reporting::diagnostic::LabelStyle::Primary,
    path,
    d.range,
  );
  codespan_reporting::diagnostic::Diagnostic::new(sev)
    .with_code(d.code.to_string())
    .with_message(d.message.clone())
    .with_labels(vec![lab])
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

struct Files<'a> {
  store: &'a paths::Store,
  input: &'a input::Input,
  analysis: &'a analysis::Analysis,
}

impl<'a> codespan_reporting::files::Files<'a> for Files<'a> {
  type FileId = paths::PathId;

  type Name = std::path::Display<'a>;

  type Source = &'a str;

  fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
    Ok(self.store.get_path(id).as_path().display())
  }

  fn source(&'a self, id: Self::FileId) -> Result<Self::Source, codespan_reporting::files::Error> {
    match self.input.sources.get(&id) {
      Some(x) => Ok(x.as_str()),
      None => Err(codespan_reporting::files::Error::FileMissing),
    }
  }

  fn line_index(
    &'a self,
    id: Self::FileId,
    byte_index: usize,
  ) -> Result<usize, codespan_reporting::files::Error> {
    let db =
      self.analysis.source_pos_db(id).ok_or(codespan_reporting::files::Error::FileMissing)?;
    let ts = text_size_util::TextSize::try_from(byte_index).unwrap();
    let pos = db.position_utf16(ts).unwrap_or(db.end_position_utf16());
    Ok(pos.line.try_into().unwrap())
  }

  fn line_range(
    &'a self,
    id: Self::FileId,
    line_index: usize,
  ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
    let db =
      self.analysis.source_pos_db(id).ok_or(codespan_reporting::files::Error::FileMissing)?;
    let start = text_pos::PositionUtf16 { line: line_index.try_into().unwrap(), col: 0 };
    let mut end = text_pos::PositionUtf16 { line: start.line + 1, col: 0 };
    let file_end = db.end_position_utf16();
    if end.line > file_end.line || end.line == file_end.line && end.col > file_end.col {
      end = file_end;
    }
    let tr =
      db.text_range_utf16(text_pos::RangeUtf16 { start, end }).expect("line range out of range");
    Ok(tr.into())
  }
}
