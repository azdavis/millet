//! Handling different kinds of SML files.

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
    let ret = match s {
      "sml" => Kind::Sml,
      "sig" => Kind::Sig,
      "fun" => Kind::Fun,
      _ => return Err(KindFromStrError(())),
    };
    Ok(ret)
  }
}
