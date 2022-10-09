//! Primitive items available in even the most minimal of environments, like `int`.
//!
//! These are not actually defined here in SML code, but the documentation for them is.

use crate::files;

/// The files.
pub const FILES: &[(&str, &str)] = files![
  "primitive/int.sml",
  "primitive/word.sml",
  "primitive/real.sml",
  "primitive/char.sml",
  "primitive/string.sml",
  "primitive/bool.sml",
  "primitive/ref.sml",
];
