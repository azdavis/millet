use pico_args::Arguments;

fn usage() {
  let current_exe_name = std::env::current_exe()
    .ok()
    .and_then(|x| x.file_name().map(|x| x.to_string_lossy().into_owned()))
    .unwrap_or_else(|| "<unknown>".to_owned());
  println!("usage:");
  println!("  {current_exe_name} [options] <path>");
  println!();
  println!("options:");
  println!("  -h, --help");
  println!("    show this help");
  println!();
  println!("arguments:");
  println!("  <path>");
  println!("    path of the project to analyze. the path should either be:");
  println!("    - a directory containing either:");
  println!("      - a single .cm or .mlb file");
  println!("      - a millet.toml config file specifying a single .cm or .mlb file");
  println!("    - a .cm or .mlb file");
}

fn run() -> bool {
  let mut args = Arguments::from_env();
  if args.contains(["-h", "--help"]) {
    usage();
    return true;
  }
  let path: String = match args.free_from_str() {
    Ok(x) => x,
    Err(e) => {
      println!("error: {e}");
      return false;
    }
  };
  let fs = paths::RealFileSystem::default();
  let mut root = match analysis::input::get_root(&fs, std::path::Path::new(path.as_str())) {
    Ok(x) => x,
    Err(e) => {
      handle_get_input_error(e);
      return false;
    }
  };
  let inp = match analysis::input::get(&fs, &mut root) {
    Ok(x) => x,
    Err(e) => {
      handle_get_input_error(e);
      return false;
    }
  };
  let mut an = analysis::Analysis::new(analysis::StdBasis::full(), config::ErrorLines::One);
  let got = an.get_many(&inp);
  let num_errors: usize = got.iter().map(|(_, errors)| errors.len()).sum();
  for (path, errors) in got {
    for e in errors {
      let path = root.as_paths().get_rel_path(path).display();
      println!(
        "{}:{}: error[{}]: {}",
        path, e.range.start, e.code, e.message
      );
    }
  }
  if num_errors == 0 {
    println!("no errors!");
    true
  } else {
    let suffix = if num_errors == 1 { "" } else { "s" };
    println!(
      "{num_errors} error{suffix}. see {} for more information",
      analysis::ERRORS_URL
    );
    false
  }
}

fn handle_get_input_error(e: analysis::input::GetInputError) {
  print!("{}", e.path().display());
  if let Some(r) = e.range() {
    print!(":{}", r.start);
  }
  let code = e.to_code();
  println!(": error[{code}]: {e}");
}

fn main() {
  if !run() {
    std::process::exit(1)
  }
}
