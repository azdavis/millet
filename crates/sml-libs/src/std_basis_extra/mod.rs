//! These are some [proposed extensions][1] to the Standard Basis Library.
//!
//! [1]: https://github.com/SMLFamily/BasisLibrary

use crate::files;

/// The files.
pub const FILES: &[(&str, &str)] = files!["fn.sml", "ref.sml", "either.sml"];
