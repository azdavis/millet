//! Tests involving type variables which may be bound implicitly, where the behavior differs between
//! some SML implementations.
//!
//! Tested with:
//!
//! - smlnj: 110.99.3
//! - mlton: 20210117

use crate::check::check;

/// - smlnj: error
/// - mlton: no error
#[test]
fn datatype() {
  check(
    r"
fun f x =
  let
    datatype t = Poly of 'a
(**                      ^^ undefined *)
  in
    Poly x; 4
  end
",
  );
}

/// - smlnj: error
/// - mlton: no error
#[test]
fn typ() {
  check(
    r"
fun f x =
  let
    type t = 'a
(**          ^^ undefined *)
  in
    x: t
  end
",
  );
}

/// smlnj and mlton: no error
#[test]
fn exception() {
  check(
    r"
fun f x =
  let
    exception Poly of 'a
  in
    raise Poly x
  end
",
  );
}
