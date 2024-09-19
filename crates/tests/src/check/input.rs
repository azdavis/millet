//! Text input with in-memory files.

use std::collections::BTreeMap;
use std::sync::LazyLock;

/// Get an input and path store from an iterator of (filename, contents).
pub(crate) fn get<'a, I>(iter: I) -> (input::Input, paths::Store)
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  _ = env_logger::builder().is_test(true).try_init();
  let mut map = BTreeMap::<paths::CleanPathBuf, String>::default();
  for (name, contents) in iter {
    let path = ROOT.as_clean_path().join(name);
    assert!(map.insert(path, contents.to_owned()).is_none(), "duplicate key: {name}");
  }
  let fs = paths::MemoryFileSystem::new(map);
  let mut store = paths::Store::new();
  let input = input::Input::new(&fs, &mut store, ROOT.as_clean_path());
  (input, store)
}

/// The real, canonical root file system path, aka `/`. Performs I/O on first access. But this
/// shouldn't fail because the root should be readable. (Otherwise, where are these tests being
/// run?)
pub(crate) static ROOT: LazyLock<paths::CleanPathBuf> =
  LazyLock::new(paths::MemoryFileSystem::root);
