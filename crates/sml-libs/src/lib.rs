//! Text of various SML libraries.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

pub mod primitive;
pub mod sml_of_nj;
pub mod smlnj_lib;
pub mod std_basis;
pub mod std_basis_extra;

macro_rules! files {
  ( $( $path:literal ),* $(,)? ) => {{
    &[
      $(
        ($path, include_str!($path)),
      )*
    ]
  }};
}

pub(crate) use files;
