//! Overloaded types.

use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum BasicOverload {
  Int,
  Real,
  Word,
  String,
  Char,
}

impl BasicOverload {
  pub(crate) fn as_str(self) -> &'static str {
    match self {
      BasicOverload::Int => "int",
      BasicOverload::Real => "real",
      BasicOverload::Word => "word",
      BasicOverload::String => "string",
      BasicOverload::Char => "char",
    }
  }
}

impl fmt::Display for BasicOverload {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.as_str())
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum CompositeOverload {
  WordInt,
  RealInt,
  Num,
  NumTxt,
  /// equality-only subset of NumTxt. used only for unification
  NumTxtEq,
}

impl CompositeOverload {
  pub(crate) fn as_basics(self) -> &'static [BasicOverload] {
    match self {
      Self::WordInt => &[BasicOverload::Word, BasicOverload::Int],
      Self::RealInt => &[BasicOverload::Real, BasicOverload::Int],
      Self::Num => &[BasicOverload::Word, BasicOverload::Real, BasicOverload::Int],
      Self::NumTxt => &[
        BasicOverload::Word,
        BasicOverload::Real,
        BasicOverload::Int,
        BasicOverload::String,
        BasicOverload::Char,
      ],
      Self::NumTxtEq => {
        &[BasicOverload::Word, BasicOverload::Int, BasicOverload::String, BasicOverload::Char]
      }
    }
  }

  /// we could also probably use `as_basics`, set intersection, then a "reverse" `as_basics` here.
  pub(crate) fn unify(self, other: Self) -> Overload {
    match (self, other) {
      (Self::WordInt, Self::WordInt | Self::Num | Self::NumTxt)
      | (Self::Num | Self::NumTxt, Self::WordInt) => Overload::Composite(Self::WordInt),
      (Self::WordInt | Self::NumTxtEq, Self::RealInt)
      | (Self::RealInt, Self::WordInt | Self::NumTxtEq) => Overload::Basic(BasicOverload::Int),
      (Self::RealInt, Self::RealInt | Self::Num | Self::NumTxt)
      | (Self::Num | Self::NumTxt, Self::RealInt) => Overload::Composite(Self::RealInt),
      (Self::Num, Self::Num | Self::NumTxt) | (Self::NumTxt, Self::Num) => {
        Overload::Composite(Self::Num)
      }
      (Self::NumTxt, Self::NumTxt) => Overload::Composite(Self::NumTxt),
      (Self::NumTxtEq, Self::NumTxtEq | Self::NumTxt) | (Self::NumTxt, Self::NumTxtEq) => {
        Overload::Composite(Self::NumTxtEq)
      }
      (Self::NumTxtEq, Self::WordInt | Self::Num) | (Self::WordInt | Self::Num, Self::NumTxtEq) => {
        Overload::Composite(Self::WordInt)
      }
    }
  }
}

impl fmt::Display for CompositeOverload {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      CompositeOverload::WordInt => f.write_str("<wordint>"),
      CompositeOverload::RealInt => f.write_str("<realint>"),
      CompositeOverload::Num => f.write_str("<num>"),
      CompositeOverload::NumTxt => f.write_str("<numtxt>"),
      CompositeOverload::NumTxtEq => f.write_str("<numtxteq>"),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Overload {
  Basic(BasicOverload),
  Composite(CompositeOverload),
}

impl Overload {
  pub(crate) fn as_basics(&self) -> &[BasicOverload] {
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

impl fmt::Display for Overload {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Overload::Basic(b) => b.fmt(f),
      Overload::Composite(c) => c.fmt(f),
    }
  }
}
