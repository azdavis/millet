//! Coverage markers.

use fast_hash::FxHashSet;
use std::sync::{LazyLock, Mutex};

thread_local! {
  static MAP: LazyLock<Mutex<FxHashSet<&'static str>>> = LazyLock::new(|| Mutex::new(FxHashSet::default()));
}

/// Hit the named marker.
///
/// This no-ops when `debug_assertions` is off.
///
/// # Panics
///
/// Upon internal error.
pub fn hit(s: &'static str) {
  if cfg!(debug_assertions) {
    MAP.with(|m| m.lock().unwrap().insert(s));
  }
}

/// Checks if the marker was previously hit.
///
/// This no-ops when `debug_assertions` is off.
///
/// # Panics
///
/// If the marker was not previously hit or internal error.
pub fn check(s: &'static str) {
  if cfg!(debug_assertions) {
    MAP.with(|m| assert!(m.lock().unwrap().contains(s), "{s} not hit"));
  }
}
