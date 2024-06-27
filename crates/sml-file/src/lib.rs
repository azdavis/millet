//! Handling different kinds of SML files.

use std::fmt;

/// A kind of SML file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
  /// Regular SML files.
  Sml,
  /// Files that contain signature definitions (usually just one).
  Sig,
  /// Files that contain functor definitions (usually just one).
  Fun,
}

impl Kind {
  /// Returns whether this is the `Sig` or `Fun` variants.
  #[must_use]
  pub fn is_sig_or_fun(&self) -> bool {
    matches!(*self, Self::Sig | Self::Fun)
  }
}

/// The error returned from `from_str` for [`Kind`].
#[derive(Debug)]
pub struct KindFromStrError(());

impl std::str::FromStr for Kind {
  type Err = KindFromStrError;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    if s.eq_ignore_ascii_case("sml") {
      return Ok(Kind::Sml);
    }
    if s.eq_ignore_ascii_case("sig") {
      return Ok(Kind::Sig);
    }
    if s.eq_ignore_ascii_case("fun") {
      return Ok(Kind::Fun);
    }
    Err(KindFromStrError(()))
  }
}

impl fmt::Display for Kind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      Kind::Sml => "sml",
      Kind::Sig => "sig",
      Kind::Fun => "fun",
    };
    f.write_str(s)
  }
}
