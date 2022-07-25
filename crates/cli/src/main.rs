use paths::FileSystem as _;
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
  let path = match fs.canonicalize(std::path::Path::new(&path)) {
    Ok(x) => x,
    Err(e) => {
      println!("{path}: error: {e}");
      return false;
    }
  };
  let (root_path, root_group) =
    match analysis::input::GroupPath::new(&fs, path.clone().into_path_buf()) {
      None => (path, None),
      Some(path) => {
        let parent = path.as_path().parent().expect("no parent");
        let rp = fs
          .canonicalize(parent)
          .expect("canonicalize parent of canonical path");
        (rp, Some(path))
      }
    };
  let mut root = paths::Root::new(root_path);
  let inp = match analysis::input::get(&fs, &mut root, root_group) {
    Ok(x) => x,
    Err(e) => {
      print!("{}", e.path().display());
      if let Some(r) = e.range() {
        print!(":{}", r.start);
      }
      let code = analysis::OTHER_ERRORS + u16::from(e.to_code());
      println!(": error[{}]: {}", code, e.message());
      return false;
    }
  };
  let mut an = analysis::Analysis::new(analysis::StdBasis::full(), config::ErrorLines::Many);
  let got = an.get_many(&inp);
  let num_errors: usize = got.iter().map(|(_, errors)| errors.len()).sum();
  for (path, errors) in got {
    for e in errors {
      let path = root.get_rel_path(path).display();
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
    println!("{num_errors} errors");
    false
  }
}

fn main() {
  if !run() {
    std::process::exit(1)
  }
}
