//! Documentation for primitives.

use fast_hash::FxHashMap;
use once_cell::sync::Lazy;

pub(crate) static DOC: Lazy<FxHashMap<&str, String>> =
  Lazy::new(|| code_h2_md_map::get(include_str!("../../../docs/primitives.md"), |_| String::new()));
