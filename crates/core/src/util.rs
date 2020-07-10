//! Miscellaneous utilities.

use std::iter::ExactSizeIterator;

/// Returns whether lhs and rhs have the same elements in the same order. Logically the same as
/// collecting both lhs and rhs into a `Vec<T>` and then checking those for equality.
pub fn eq_iter<I, T>(mut lhs: I, mut rhs: I) -> bool
where
  I: ExactSizeIterator<Item = T>,
  T: Eq,
{
  if lhs.len() != rhs.len() {
    return false;
  }
  loop {
    match (lhs.next(), rhs.next()) {
      (Some(lhs), Some(rhs)) => {
        if lhs != rhs {
          return false;
        }
      }
      (Some(_), None) | (None, Some(_)) => return false,
      (None, None) => return true,
    }
  }
}
