//! Coverage markers.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use fast_hash::FxHashSet;
use once_cell::sync::Lazy;
use std::sync::Mutex;

thread_local! {
  static MAP: Lazy<Mutex<FxHashSet<&'static str>>> = Lazy::new(|| Mutex::new(FxHashSet::default()));
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
