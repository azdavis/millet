//! Primitive items available in even the most minimal of environments, like `int`.
//!
//! These are not actually defined here in SML code, but the documentation for them is.

use crate::files;

/// The files.
pub const FILES: &[(&str, &str)] = files!["primitive.sml"];
