use anyhow::{bail, Result};
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
  println!("    directory path of the project to analyze. the directory should contain either a");
  println!("    single .cm file or a millet.toml config file.");
}

fn main() -> Result<()> {
  let mut args = Arguments::from_env();
  if args.contains(["-h", "--help"]) {
    usage();
    return Ok(());
  }
  let root: String = args.free_from_str()?;
  let fs = paths::RealFileSystem::default();
  let mut root = paths::Root::new(fs.canonicalize(std::path::Path::new(&root))?);
  let inp = analysis::get_input(&fs, &mut root)?;
  let mut an = analysis::Analysis::new(analysis::StdBasis::full());
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
    println!("no errors");
    Ok(())
  } else {
    bail!("{num_errors} errors");
  }
}
