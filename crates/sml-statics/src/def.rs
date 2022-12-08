//! Def-related types.

/// A definition site.
#[derive(Debug, Clone, Copy)]
pub enum Def {
  /// A def contained at a path.
  Path(Path, sml_hir::Idx),
  /// A primitive, inherent def.
  Primitive(Primitive),
}

/// A definition path.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Path {
  /// A regular path.
  Regular(paths::PathId),
  /// A built-in library path, like the std basis or other such similar "always available"
  /// libraries. Contrast with primitives, which are built-in but not expressible in a regular SML
  /// source file.
  BuiltinLib(&'static str),
}

/// A primitive definition, often not expressible in real SML.
#[derive(Debug, Clone, Copy)]
pub struct Primitive(PrimitiveKind);

impl Primitive {
  /// Returns this as a str.
  #[must_use]
  pub fn as_str(self) -> &'static str {
    self.0.as_str()
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum PrimitiveKind {
  Int,
  Word,
  Real,
  Char,
  String,
  Bool,
  True,
  False,
  List,
  Nil,
  Cons,
  RefTy,
  RefVal,
  Unit,
  Exn,
  Mul,
  Add,
  Sub,
  RealDiv,
  Lt,
  LtEq,
  Gt,
  GtEq,
  Neg,
  Abs,
  Div,
  Mod,
  Eq,
  Neq,
  Use,
}

impl PrimitiveKind {
  pub(crate) fn as_str(self) -> &'static str {
    match self {
      Self::Int => "int",
      Self::Word => "word",
      Self::Real => "real",
      Self::Char => "char",
      Self::String => "string",
      Self::Bool => "bool",
      Self::True => "true",
      Self::False => "false",
      Self::List => "list",
      Self::Nil => "nil",
      Self::Cons => "::",
      Self::RefTy | Self::RefVal => "ref",
      Self::Unit => "unit",
      Self::Exn => "exn",
      Self::Mul => "*",
      Self::Add => "+",
      Self::Sub => "-",
      Self::RealDiv => "/",
      Self::Lt => "<",
      Self::LtEq => "<=",
      Self::Gt => ">",
      Self::GtEq => ">=",
      Self::Neg => "~",
      Self::Abs => "abs",
      Self::Div => "div",
      Self::Mod => "mod",
      Self::Eq => "=",
      Self::Neq => "<>",
      Self::Use => "use",
    }
  }
}

impl From<PrimitiveKind> for Def {
  fn from(val: PrimitiveKind) -> Self {
    Self::Primitive(Primitive(val))
  }
}
