//! These are some [proposed extensions][1] to the Standard Basis Library.
//!
//! [1]: https://github.com/SMLFamily/BasisLibrary

use crate::files;

/// The files.
pub const FILES: &[(&str, &str)] =
  files!["std_basis_extra/fn.sml", "std_basis_extra/ref.sml", "std_basis_extra/either.sml"];
