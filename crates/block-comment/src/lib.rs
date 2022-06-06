//! Nested block comments delimited with `(*` and `*)`.

pub struct Consumed;

pub enum Unmatched {
  Open,
  Close,
}

/// requires `bs.get(*idx) == Some(&b)`.
pub fn get(idx: &mut usize, b: u8, bs: &[u8]) -> Result<Option<Consumed>, Unmatched> {
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
        (_, None) => return Err(Unmatched::Open),
        (None, Some(_)) => unreachable!(),
      }
    }
  }
  if b == b'*' && bs.get(*idx + 1) == Some(&b')') {
    *idx += 2;
    Err(Unmatched::Close)
  } else {
    Ok(None)
  }
}
