//! Empty constructs and their errors.

use crate::check::{check, check_with_warnings};

#[test]
fn fun() {
  check(
    r"
    fun f = 3
(** ^^^^^^^^^ `fun` requires at least 1 parameter *)
",
  );
}

#[test]
fn include() {
  check(
    r"
signature S = sig
    include
(** ^^^^^^^ requires at least 1 operand *)
end
",
  );
}

#[test]
fn open() {
  check(
    r"
    open
(** ^^^^ requires at least 1 operand *)
",
  );
}

#[test]
fn let_in_end() {
  check(
    r"
val _ = let in end
(**     ^^^ requires at least 1 expression *)
",
  );
}

#[test]
fn let_in() {
  check_with_warnings(
    r"
val _ = let in 3 end
(** + overly complex *)
",
  );
}

#[test]
fn let_structure() {
  check_with_warnings(
    r"
structure S = let in struct val x = 4 end end
(** + overly complex *)
",
  );
}

#[test]
fn local_top() {
  check_with_warnings(
    r"
local in val x = 3 end
(** + overly complex *)
",
  );
}

#[test]
fn local_in_let() {
  check_with_warnings(
    r"
val _ =
  let
    local in val x = 3 end
(** + overly complex *)
  in
    4
  end
",
  );
}

#[test]
fn record_exp() {
  check_with_warnings(
    r"
val x = {}
(**     ^^ usually written as `()` *)
",
  );
}

#[test]
fn record_pat() {
  check_with_warnings(
    r"
fun get {} = 3
(**     ^^ usually written as `()` *)
",
  );
}

#[test]
fn record_ty() {
  check_with_warnings(
    r"
val x : {} = ()
(**     ^^ usually written as `unit` *)
",
  );
}
