//! Text input with in-memory files.

use fast_hash::FxHashMap;
use once_cell::sync::Lazy;
use paths::FileSystem as _;
use std::path::PathBuf;

/// Get an input and path store from an iterator of (filename, contents).
pub(crate) fn get<'a, I>(iter: I) -> (input::Input, paths::Store)
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  _ = env_logger::builder().is_test(true).try_init();
  let mut map = FxHashMap::<PathBuf, String>::default();
  for (name, contents) in iter {
    let mut buf = ROOT.as_path().to_owned();
    buf.push(name);
    assert!(map.insert(buf, contents.to_owned()).is_none(), "duplicate key: {name}");
  }
  let fs = paths::MemoryFileSystem::new(map);
  let mut store = paths::Store::new();
  let input = input::Input::new(&fs, &mut store, &ROOT);
  (input, store)
}

/// The real, canonical root file system path, aka `/`. Performs I/O on first access. But this
/// shouldn't fail because the root should be readable. (Otherwise, where are these tests being
/// run?)
pub(crate) static ROOT: Lazy<paths::CanonicalPathBuf> =
  Lazy::new(|| paths::RealFileSystem::default().canonical(std::path::Path::new("/")).unwrap());
