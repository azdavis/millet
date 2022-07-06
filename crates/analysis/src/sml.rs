pub(crate) mod std_basis;

macro_rules! files {
  ( $( $x:literal , )* ) => {{
    &[
      $(
        ($x, include_str!($x)),
      )*
    ]
  }};
}

pub(crate) use files;
