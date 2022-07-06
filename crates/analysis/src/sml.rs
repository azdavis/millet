pub(crate) mod sml_nj;
pub(crate) mod std_basis;
pub(crate) mod std_basis_extra;

macro_rules! files {
  ( $( $x:literal ),* $(,)? ) => {{
    &[
      $(
        ($x, include_str!($x)),
      )*
    ]
  }};
}

pub(crate) use files;
