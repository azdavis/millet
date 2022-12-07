//! Transforming a Markdown file with lots of level-2 headings, each documenting some `code`, into
//! a hash map.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use fast_hash::FxHashMap;

/// Does the transformation. `f` returns the starting string for each key.
///
/// # Panics
///
/// If there were duplicate level-2 headings.
pub fn get<F>(md: &str, mut f: F) -> FxHashMap<&str, String>
where
  F: FnMut(&str) -> String,
{
  let mut ret = FxHashMap::<&str, String>::default();
  let mut key = None::<&str>;
  let mut val = String::new();
  #[allow(clippy::single_match_else)]
  for line in md.lines() {
    match code_h2(line) {
      Some(next) => {
        if let Some(key) = key {
          assert!(ret.insert(key, val).is_none());
        }
        key = Some(next);
        val = f(next);
      }
      None => {
        val.push('\n');
        val.push_str(line);
      }
    }
  }
  if let Some(key) = key {
    assert!(ret.insert(key, val).is_none());
  }
  ret
}

fn code_h2(s: &str) -> Option<&str> {
  s.strip_prefix("## `")?.strip_suffix('`')
}
