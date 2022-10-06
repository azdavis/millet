//! "Old" SML NJ libraries, from the [SML of NJ][1] doc page.
//!
//! [1]: https://www.smlnj.org/doc/SMLofNJ/pages/index-all.html

use crate::files;

/// The files.
pub const FILES: &[(&str, &str)] = files![
  "cinterface.sml",
  "cleanup.sml",
  "cont.sml",
  "gc.sml",
  "object.sml",
  "poll.sml",
  "unsafe-array.sml",
  "unsafe-mono-array.sml",
  "unsafe-mono-vector.sml",
  "unsafe-vector.sml",
  "unsafe.sml",
  "prof-control.sml",
  "internals.sml",
  "interval-timer.sml",
  "signals.sml",
  "sysinfo.sml",
  "susp.sml",
  "weak.sml",
  "smlnj.sml",
  "unix-signals.sml",
];
