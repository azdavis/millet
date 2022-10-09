//! "Old" SML NJ libraries, from the [SML of NJ][1] doc page.
//!
//! [1]: https://www.smlnj.org/doc/SMLofNJ/pages/index-all.html

use crate::files;

/// The files.
pub const FILES: &[(&str, &str)] = files![
  "sml_of_nj/cinterface.sml",
  "sml_of_nj/cleanup.sml",
  "sml_of_nj/cont.sml",
  "sml_of_nj/gc.sml",
  "sml_of_nj/object.sml",
  "sml_of_nj/poll.sml",
  "sml_of_nj/unsafe-array.sml",
  "sml_of_nj/unsafe-mono-array.sml",
  "sml_of_nj/unsafe-mono-vector.sml",
  "sml_of_nj/unsafe-vector.sml",
  "sml_of_nj/unsafe.sml",
  "sml_of_nj/prof-control.sml",
  "sml_of_nj/internals.sml",
  "sml_of_nj/interval-timer.sml",
  "sml_of_nj/signals.sml",
  "sml_of_nj/sysinfo.sml",
  "sml_of_nj/susp.sml",
  "sml_of_nj/weak.sml",
  "sml_of_nj/smlnj.sml",
  "sml_of_nj/unix-signals.sml",
];
