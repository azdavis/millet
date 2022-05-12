//! Parsing for LSP headers.

#[cfg(test)]
mod tests;

pub fn content_length(bs: &[u8]) -> Option<usize> {
  let mut i = bs.iter().position(|&b| b == b':')?;
  if &bs[0..i] != b"Content-Length" {
    return None;
  }
  i += 1;
  while *bs.get(i)? == b' ' {
    i += 1;
  }
  let start = i;
  while bs.get(i)?.is_ascii_digit() {
    i += 1;
  }
  if start == i || *bs.get(i)? != b'\r' || *bs.get(i + 1)? != b'\n' || i + 2 != bs.len() {
    return None;
  }
  let num = std::str::from_utf8(&bs[start..i]).ok()?;
  Some(num.parse().unwrap())
}
