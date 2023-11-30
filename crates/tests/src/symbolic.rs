//! Symbolic names.

use crate::check::check;

#[test]
fn symbolic_structure() {
  check(
    r"
structure % = struct end
",
  );
}

#[test]
fn symbolic_signature() {
  check(
    r"
signature % = sig end
",
  );
}

#[test]
fn symbolic_functor() {
  check(
    r"
functor % () = struct end
",
  );
}
