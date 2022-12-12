//! Text input with in-memory files.

use fast_hash::FxHashMap;
use once_cell::sync::Lazy;
use paths::FileSystem as _;

pub(crate) fn get<'a, I>(files: I) -> (input::Result, paths::Store)
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  let _ = env_logger::builder().is_test(true).try_init();
  let map: FxHashMap<_, _> = files
    .into_iter()
    .map(|(name, contents)| {
      let mut buf = ROOT.as_path().to_owned();
      buf.push(name);
      (buf, contents.to_owned())
    })
    .collect();
  let fs = paths::MemoryFileSystem::new(map);
  let mut store = paths::Store::new();
  let input = input::Input::new(&fs, &mut store, &ROOT);
  (input, store)
}

/// The real, canonical root file system path, aka `/`. Performs IO on first access. But this
/// shouldn't fail because the root should be readable. (Otherwise, where are these tests being
/// run?)
pub(crate) static ROOT: Lazy<paths::CanonicalPathBuf> =
  Lazy::new(|| paths::RealFileSystem::default().canonicalize(std::path::Path::new("/")).unwrap());
