//! Overloaded types.

use std::fmt;

/// A basic overload.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Basic {
  /// The int overload, e.g. `Int32`, `Int64`.
  Int,
  /// The real overload, e.g. `Real32`, `Real64`.
  Real,
  /// The word overload, e.g. `Word8`, `Word16`.
  Word,
  /// The string overload. There is only one actual string type, but it is used in other overloads
  /// like `<numtxt>`.
  String,
  /// The string overload. There is only one actual char type, but it is used in other overloads
  /// like `<numtxt>`.
  Char,
}

impl Basic {
  /// Returns this as a string.
  #[must_use]
  pub fn as_str(self) -> &'static str {
    match self {
      Basic::Int => "int",
      Basic::Real => "real",
      Basic::Word => "word",
      Basic::String => "string",
      Basic::Char => "char",
    }
  }
}

impl fmt::Display for Basic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.as_str())
  }
}

/// A composite overload.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Composite {
  /// Words and ints.
  WordInt,
  /// Reals and ints.
  RealInt,
  /// All kinds of numbers.
  Num,
  /// equality-only subset of `NumTxt`. not explicitly mentioned in the Definition. used only for
  /// unification.
  WordIntTxt,
  /// All kinds of numbers and textual types (char and string).
  NumTxt,
}

impl Composite {
  pub(crate) fn as_basics(self) -> &'static [Basic] {
    match self {
      Self::WordInt => &[Basic::Word, Basic::Int],
      Self::RealInt => &[Basic::Real, Basic::Int],
      Self::Num => &[Basic::Word, Basic::Real, Basic::Int],
      Self::WordIntTxt => &[Basic::Word, Basic::Int, Basic::String, Basic::Char],
      Self::NumTxt => &[Basic::Word, Basic::Real, Basic::Int, Basic::String, Basic::Char],
    }
  }

  /// we could also probably use `as_basics`, set intersection, then a "reverse" `as_basics` here.
  pub(crate) fn unify(self, other: Self) -> Overload {
    match (self, other) {
      (Self::WordInt, Self::WordInt | Self::Num | Self::NumTxt)
      | (Self::Num | Self::NumTxt, Self::WordInt)
      | (Self::WordIntTxt, Self::WordInt | Self::Num)
      | (Self::WordInt | Self::Num, Self::WordIntTxt) => Self::WordInt.into(),
      (Self::WordInt | Self::WordIntTxt, Self::RealInt)
      | (Self::RealInt, Self::WordInt | Self::WordIntTxt) => Basic::Int.into(),
      (Self::RealInt, Self::RealInt | Self::Num | Self::NumTxt)
      | (Self::Num | Self::NumTxt, Self::RealInt) => Self::RealInt.into(),
      (Self::Num, Self::Num | Self::NumTxt) | (Self::NumTxt, Self::Num) => Self::Num.into(),
      (Self::WordIntTxt, Self::WordIntTxt | Self::NumTxt) | (Self::NumTxt, Self::WordIntTxt) => {
        Self::WordIntTxt.into()
      }
      (Self::NumTxt, Self::NumTxt) => Self::NumTxt.into(),
    }
  }
}

impl fmt::Display for Composite {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Composite::WordInt => f.write_str("<wordint>"),
      Composite::RealInt => f.write_str("<realint>"),
      Composite::Num => f.write_str("<num>"),
      Composite::WordIntTxt => f.write_str("<wordinttxt>"),
      Composite::NumTxt => f.write_str("<numtxt>"),
    }
  }
}

/// An overload.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Overload {
  /// A basic overload, like `int`.
  Basic(Basic),
  /// A composite overload, like `<numtxt>`.
  Composite(Composite),
}

impl Overload {
  pub(crate) fn as_basics(&self) -> &[Basic] {
    match self {
      Overload::Basic(b) => std::slice::from_ref(b),
      Overload::Composite(c) => c.as_basics(),
    }
  }

  /// returns `None` iff the overloads could not be unified.
  pub(crate) fn unify(self, other: Self) -> Option<Self> {
    match (self, other) {
      (Self::Basic(b1), Self::Basic(b2)) => {
        if b1 == b2 {
          Some(Self::Basic(b1))
        } else {
          None
        }
      }
      (Self::Basic(b), Self::Composite(c)) | (Self::Composite(c), Self::Basic(b)) => {
        c.as_basics().iter().find(|&&x| x == b).copied().map(Self::Basic)
      }
      (Self::Composite(c1), Self::Composite(c2)) => Some(c1.unify(c2)),
    }
  }
}

impl From<Basic> for Overload {
  fn from(val: Basic) -> Self {
    Self::Basic(val)
  }
}

impl From<Composite> for Overload {
  fn from(val: Composite) -> Self {
    Self::Composite(val)
  }
}

impl fmt::Display for Overload {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Overload::Basic(b) => b.fmt(f),
      Overload::Composite(c) => c.fmt(f),
    }
  }
}
