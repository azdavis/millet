use crate::check::check;

#[test]
fn not_arrow_type() {
  check(
    r#"
val _ = fn nil _ => 1 | _ => 2
(**        ^^^^^ mismatched types: expected an arrow type, found '26 list *)
"#,
  );
}

#[test]
fn not_cons_type() {
  check(
    r#"
val _ = fn op:: => 3
(**        ^^^^ mismatched types: expected a constructor type, found '25 * '25 list -> '25 list *)
"#,
  );
}

#[test]
fn wrong_id_status() {
  check(
    r#"
val C = 3
val _ =
  case 3 of
    C _ => 1
(** ^ mismatched identifier status: expected constructor or exception, found value *)
  | _ => 2
"#,
  );
}

#[test]
fn real() {
  check(
    r#"
val _ =
  case 123.123 of
    1.2 => 1
(** ^^^ real constant used as a pattern *)
  | _ => 2
"#,
  );
}
