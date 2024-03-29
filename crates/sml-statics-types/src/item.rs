//! See [`Item`].

use std::fmt;

/// A kind of SML language construct.
#[derive(Debug, Clone, Copy)]
pub enum Item {
  /// A value.
  Val,
  /// A type.
  Ty,
  /// A type variable.
  TyVar,
  /// A structure.
  Struct,
  /// A signature.
  Sig,
  /// A functor.
  Functor,
}

impl fmt::Display for Item {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Item::Val => f.write_str("value"),
      Item::Ty => f.write_str("type"),
      Item::TyVar => f.write_str("type variable"),
      Item::Struct => f.write_str("structure"),
      Item::Sig => f.write_str("signature"),
      Item::Functor => f.write_str("functor"),
    }
  }
}
