/// Configuration.
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct Cfg {
  /// Mark things as defined.
  ///
  /// This is for then emitting warnings for things that were marked as defined but then not used.
  pub(crate) mark_defined: bool,
}
