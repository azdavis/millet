//! Def-related types.

/// A definition site.
#[derive(Debug, Clone, Copy)]
pub enum Def {
  /// A def contained at a path.
  Path(Path, sml_hir::Idx),
  /// A primitive, inherent def.
  Primitive(&'static str),
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
