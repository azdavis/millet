//! Documentation for primitives.

use fast_hash::FxHashMap;
use once_cell::sync::Lazy;

fn code_h2(s: &str) -> Option<&str> {
  s.strip_prefix("## `")?.strip_suffix('`')
}

pub(crate) static DOC: Lazy<FxHashMap<&str, String>> = Lazy::new(|| {
  let mut ret = FxHashMap::<&str, String>::default();
  let mut key = None::<&str>;
  let mut val = String::new();
  #[allow(clippy::single_match_else)]
  for line in include_str!("../../../docs/primitives.md").lines() {
    match code_h2(line) {
      Some(next) => {
        if let Some(key) = key {
          assert!(ret.insert(key, val).is_none());
        }
        key = Some(next);
        val = String::new();
      }
      None => {
        val.push('\n');
        val.push_str(line);
      }
    }
  }
  assert!(ret.insert(key.unwrap(), val).is_none());
  ret
});
