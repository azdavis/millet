//! See [`Mode`].

/// The mode for checking.
#[derive(Debug, Clone, Copy)]
pub enum Mode {
  /// Regular checking. The default.
  Regular(Option<paths::PathId>),
  /// Built-in library checking. Notably, ascription structure expressions will not check to see if
  /// they actually match the signature.
  ///
  /// The string is the name of the built-in library file.
  BuiltinLib(&'static str),
  /// Only used for path ordering.
  ///
  /// Since path ordering only cares about structure-level name resolution, we can skip lots of
  /// statics checks in this mode for better performance. We probably don't actually skip all the
  /// checks that we conceivably could skip, but that's ok from a correctness standpoint.
  PathOrder,
  /// Populate extra info for running the dynamics.
  Dynamics,
}

impl Mode {
  /// Returns whether this is the "path order" mode.
  #[must_use]
  pub fn is_path_order(&self) -> bool {
    matches!(self, Mode::PathOrder)
  }
}
