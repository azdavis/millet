//! Displaying some types.

use crate::dynamics::Dynamics;
use std::fmt;

impl fmt::Display for Dynamics<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Dynamics { .. }")
  }
}
