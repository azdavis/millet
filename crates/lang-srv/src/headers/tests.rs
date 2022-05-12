use super::content_length as cl;

#[test]
fn bad() {
  assert_eq!(cl(b""), None);
  assert_eq!(cl(b"Content-Length: 123"), None);
  assert_eq!(cl(b"Content-Length: \r\n 123\r\n"), None);
  assert_eq!(cl(b"Content-Length: 123\r\n\r\n"), None);
  assert_eq!(cl(b"Content-Length 123\r\n"), None);
  assert_eq!(cl(b"Content-Length: 123\n"), None);
  assert_eq!(cl(b"Content-Length: 123\r"), None);
  assert_eq!(cl(b" Content-Length: 123\r\n"), None);
  assert_eq!(cl(b"Content-Type: 123\r\n"), None);
  assert_eq!(cl(b"Content-Length: \r\n"), None);
}

#[test]
fn good() {
  assert_eq!(cl(b"Content-Length:123\r\n"), Some(123));
  assert_eq!(cl(b"Content-Length: 456\r\n"), Some(456));
  assert_eq!(cl(b"Content-Length:    789\r\n"), Some(789));
}
