//! Pervasive types.

use fast_hash::FxHashMap;

/// A mapping to override diagnostic severity.
pub type Severities = FxHashMap<diagnostic::Code, Option<diagnostic::Severity>>;

/// A description of how to check a group of source files.
#[derive(Debug)]
pub struct Group {
  /// A lowered BasDec, describing the group.
  pub bas_dec: mlb_hir::BasDec,
  /// A position DB for the group file that yielded the dec.
  pub pos_db: text_pos::PositionDb,
}
