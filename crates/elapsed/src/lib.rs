//! See [`time`].

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

use std::time::Instant;

/// Calls `f` and logs the time it took to do so.
pub fn time<F, T>(msg: &str, f: F) -> T
where
  F: FnOnce() -> T,
{
  let start = Instant::now();
  let ret = f();
  let elapsed = Instant::now().duration_since(start);
  log::info!("{msg}: {elapsed:?}");
  ret
}
