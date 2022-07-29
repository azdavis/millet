//! Utilities.

#![deny(missing_debug_implementations, missing_docs, rust_2018_idioms)]

pub mod block_comment;

/// Returns whether `b` is a whitespace character for our purposes.
pub fn is_whitespace(b: u8) -> bool {
  b.is_ascii_whitespace() || b == 0xb
}

/// Advances `idx` until `bs` is out of bytes or `p` no longer holds.
pub fn advance_while<P>(idx: &mut usize, bs: &[u8], p: P)
where
  P: Fn(u8) -> bool,
{
  while let Some(&b) = bs.get(*idx) {
    if p(b) {
      *idx += 1;
    } else {
      break;
    }
  }
}
