//! Empty constructs and their errors.

use crate::check::check;

#[test]
fn fun() {
  check(
    r#"
    fun f = 3
(** ^^^^^^^^^ `fun` requires at least 1 parameter *)
"#,
  );
}

#[test]
fn include() {
  check(
    r#"
signature S = sig
    include
(** ^^^^^^^ requires at least 1 operand *)
end
"#,
  );
}

#[test]
fn open() {
  check(
    r#"
    open
(** ^^^^ requires at least 1 operand *)
"#,
  );
}

#[test]
fn let_in_end() {
  check(
    r#"
val _ = let in end
(**     ^^^^^^^^^^ requires at least 1 expression *)
"#,
  );
}
