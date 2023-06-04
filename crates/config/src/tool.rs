//! See [`Tool`].

use serde::Deserialize;

/// A default-`true` `bool`.
///
/// Handy for when you have a type containing a `bool` that you want to derive `Deserialize` on, but
/// you also want the `bool` to default to `true` if not provided when deserializing.
///
/// In such a case, you can have the field be type `Tool` instead of `bool`, and then tag the field
/// with `#[serde(default)]`.
#[derive(Debug, Clone, Copy, Deserialize)]
pub struct Tool(pub bool);

impl Default for Tool {
  fn default() -> Self {
    Self(true)
  }
}

impl std::ops::Not for Tool {
  type Output = bool;

  fn not(self) -> Self::Output {
    !self.0
  }
}
