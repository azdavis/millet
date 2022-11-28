//! Handling nested block comments delimited with `(*` and `*)`.

/// A marker signifying a block comment was consumed.
#[derive(Debug)]
pub struct Consumed;

/// An error for an unclosed comment.
#[derive(Debug)]
pub struct UnclosedError;

/// Requires `bs.get(*idx) == Some(&b)`.
///
/// # Errors
///
/// If the comment was not closed.
pub fn get(idx: &mut usize, b: u8, bs: &[u8]) -> Result<Option<Consumed>, UnclosedError> {
  debug_assert_eq!(bs.get(*idx), Some(&b));
  if b == b'(' && bs.get(*idx + 1) == Some(&b'*') {
    *idx += 2;
    let mut level = 1_usize;
    loop {
      match (bs.get(*idx), bs.get(*idx + 1)) {
        (Some(&b'('), Some(&b'*')) => {
          *idx += 2;
          level += 1;
        }
        (Some(&b'*'), Some(&b')')) => {
          *idx += 2;
          level -= 1;
          if level == 0 {
            return Ok(Some(Consumed));
          }
        }
        (Some(_), Some(_)) => *idx += 1,
        (_, None) => return Err(UnclosedError),
        (None, Some(_)) => unreachable!("cannot have a byte after EOF"),
      }
    }
  }
  Ok(None)
}
