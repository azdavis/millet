use super::hex;

#[test]
fn digit() {
  assert_eq!(hex(b'0'), Some(0));
  assert_eq!(hex(b'1'), Some(1));
  assert_eq!(hex(b'2'), Some(2));
  assert_eq!(hex(b'3'), Some(3));
  assert_eq!(hex(b'4'), Some(4));
  assert_eq!(hex(b'5'), Some(5));
  assert_eq!(hex(b'6'), Some(6));
  assert_eq!(hex(b'7'), Some(7));
  assert_eq!(hex(b'8'), Some(8));
  assert_eq!(hex(b'9'), Some(9));
  assert_eq!(hex(b'9'), Some(9));
}

#[test]
fn lower() {
  assert_eq!(hex(b'a'), Some(10));
  assert_eq!(hex(b'b'), Some(11));
  assert_eq!(hex(b'c'), Some(12));
  assert_eq!(hex(b'd'), Some(13));
  assert_eq!(hex(b'e'), Some(14));
  assert_eq!(hex(b'f'), Some(15));
}

#[test]
fn upper() {
  assert_eq!(hex(b'A'), Some(10));
  assert_eq!(hex(b'B'), Some(11));
  assert_eq!(hex(b'C'), Some(12));
  assert_eq!(hex(b'D'), Some(13));
  assert_eq!(hex(b'E'), Some(14));
  assert_eq!(hex(b'F'), Some(15));
}

#[test]
fn other() {
  assert_eq!(hex(b'.'), None);
  assert_eq!(hex(b'-'), None);
  assert_eq!(hex(b'+'), None);
  assert_eq!(hex(b'G'), None);
  assert_eq!(hex(b'*'), None);
  assert_eq!(hex(b'?'), None);
}
