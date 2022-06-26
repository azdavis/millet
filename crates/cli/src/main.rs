use anyhow::{bail, Result};
use paths::FileSystem as _;

fn main() -> Result<()> {
  let fs = paths::RealFileSystem::default();
  let root = fs.canonicalize(std::env::current_dir()?.as_path())?;
  let mut root = paths::Root::new(root);
  let inp = analysis::get_input(&fs, &mut root)?;
  let mut an = analysis::Analysis::new(analysis::StdBasis::Full);
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
