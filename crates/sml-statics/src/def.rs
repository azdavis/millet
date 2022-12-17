//! Def-related types.

use fast_hash::FxHashMap;
use once_cell::sync::Lazy;

/// A definition site.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Def {
  /// A def contained at a path.
  Path(Path, sml_hir::Idx),
  /// A primitive, inherent def.
  Primitive(Primitive),
}

impl Def {
  /// Returns this as a SML HIR idx at a regular path.
  #[must_use]
  pub fn to_regular_idx(self) -> Option<paths::WithPath<sml_hir::Idx>> {
    match self {
      Def::Path(p, idx) => match p {
        Path::Regular(p) => Some(p.wrap(idx)),
        Path::BuiltinLib(_) => None,
      },
      Def::Primitive(_) => None,
    }
  }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Primitive(PrimitiveKind);

impl Primitive {
  /// Returns Markdown documentation for this.
  ///
  /// # Panics
  ///
  /// If there was no documentation for this.
  #[must_use]
  pub fn doc(self) -> &'static str {
    // @test(repo::primitives)
    PRIMITIVE_DOC.get(&self.0).expect("no doc for this primitive").as_ref()
  }
}

static PRIMITIVE_DOC: Lazy<FxHashMap<PrimitiveKind, String>> = Lazy::new(|| {
  let raw = code_h2_md_map::get(include_str!("../../../docs/primitives.md"), |_| String::new());
  raw.into_iter().map(|(k, v)| (k.parse().expect("not a primitive kind"), v)).collect()
});

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl std::str::FromStr for PrimitiveKind {
  type Err = String;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let ret = match s {
      // @primitives(start)
      "type int" => Self::Int,
      "type word" => Self::Word,
      "type real" => Self::Real,
      "type char" => Self::Char,
      "type string" => Self::String,
      "type bool" => Self::Bool,
      "val true" => Self::True,
      "val false" => Self::False,
      "type 'a list" => Self::List,
      "val nil" => Self::Nil,
      "val op ::" => Self::Cons,
      "type 'a ref" => Self::RefTy,
      "val ref" => Self::RefVal,
      "type unit" => Self::Unit,
      "type exn" => Self::Exn,
      "val op *" => Self::Mul,
      "val op +" => Self::Add,
      "val op -" => Self::Sub,
      "val op /" => Self::RealDiv,
      "val op <" => Self::Lt,
      "val op <=" => Self::LtEq,
      "val op >" => Self::Gt,
      "val op >=" => Self::GtEq,
      "val op ~" => Self::Neg,
      "val abs" => Self::Abs,
      "val div" => Self::Div,
      "val mod" => Self::Mod,
      "val op =" => Self::Eq,
      "val op <>" => Self::Neq,
      "val use" => Self::Use,
      // @primitives(end)
      _ => return Err(s.to_owned()),
    };
    Ok(ret)
  }
}
