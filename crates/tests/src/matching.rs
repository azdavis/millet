use crate::check::{check, fail};

#[test]
fn smoke_case() {
  check(
    r#"
val _ =
  case 3 of
(**    ^ non-exhaustive case: missing _ *)
    4 => 5
"#,
  );
}

#[test]
fn smoke_binding_int() {
  check(
    r#"
val 3 = 1 + 2
(** ^ non-exhaustive binding: missing _ *)
"#,
  );
}

#[test]
fn smoke_binding_datatype() {
  check(
    r#"
datatype bin = Zero | One
val One = One
(** ^^^ non-exhaustive binding: missing Zero *)
"#,
  );
}

#[test]
fn big_datatype_missing() {
  check(
    r#"
datatype hmm = A | B of int | C of hmm | D of string
val _ =
  case A of
(**    ^ non-exhaustive case: missing C (C (C (C _))), C (C (C (B _))), and 6 others *)
    A => 0
  | B 1 => 1
  | B 3 => 2
  | B 123 => 3
  | C A => 4
  | D "foo" => 5
  | D "bar" => 6
  | C (C (C A)) => 7
  | D "quz" => 8
  | C (D "guh") => 9
  | D _ => 10
  | C (B 3) => 11
  | C (B n) => n
  | C (D "hey") => 13
  | B 234 => 14
"#,
  );
}

#[test]
fn int_tuple() {
  check(
    r#"
val _ =
  case (1, 2) of
(**    ^^^^^^ non-exhaustive case: missing (_, _) *)
    (3, 4) => 0
  | (x, 6) => x
"#,
  );
}

#[test]
fn big_ok() {
  check(
    r#"
datatype abc = A | B | C
val _ =
  case (3, "hey", false, A) of
    (3, "guh", _, _) => 0
  | (_, "hey", false, C) => 1
  | (4, _, false, _) => 2
  | (_, _, false, B) => 3
  | (_, _, false, _) => 4
  | (3, _, true, A) => 5
  | (_, "nope", true, A) => 6
  | (n, _, true, A) => n
  | (_, _, true, B) => 8
  | (_, _, true, C) => 9
"#,
  );
}

#[test]
fn bool_tuple() {
  check(
    r#"
val _ =
  case (true, false) of
(**    ^^^^^^^^^^^^^ non-exhaustive case: missing (false, true) *)
    (true, _) => 0
  | (_, false) => 1
"#,
  );
}

#[test]
fn big_tuple() {
  check(
    r#"
datatype abc = A | B | C
val _ =
  case (1, 3.3, "hey", A, false) of
    (1, _, _, A, _) => 0
  | (3, x, "foo", C, true) => 1
  | (_, _, "nope", B, _) => 2
  | (_, _, "guy", B, _) => 3
  | (_, _, "thing", B, _) => 4
  | (_, _, _, B, true) => 5
  | (x, _, _, B, false) => x
  | (_, _, _, A, true) => 5
  | (x, _, _, A, false) => x
  | (_, _, _, C, true) => 5
  | (x, _, _, C, false) => x
"#,
  );
}

#[test]
fn unreachable_tuple() {
  check(
    r#"
val _ =
  case (true, false) of
    (_, false) => 1
  | (_, false) => 2
(**  ^ unreachable pattern *)
  | _ => 3
"#,
  );
}

#[test]
fn unreachable_int() {
  check(
    r#"
val _ =
  case 3 of
    4 => 1
  | 4 => 2
(** ^ unreachable pattern *)
  | _ => 3
"#,
  );
}

#[test]
fn reachable_tuple() {
  check(
    r#"
val _ =
  case (9, 7) of
    (3, 8) => 0
  | (3, _) => 5
  | _ => 6
"#,
  );
}

#[test]
fn big_datatype_ok() {
  check(
    r#"
datatype hmm = A | B of int | C of hmm | D of string
val _ =
  case A of
    A => 0
  | B 1 => 1
  | B 3 => 2
  | B 123 => 3
  | C A => 4
  | D "foo" => 5
  | D "bar" => 6
  | C (C (C A)) => 7
  | D "quz" => 8
  | C (D "guh") => 9
  | D _ => 10
  | C (B 3) => 11
  | C (B n) => n
  | C (D "hey") => 13
  | B _ => 14
  | C (C (C _)) => 15
  | C _ => 16
"#,
  );
}

#[test]
fn list_ok() {
  check(
    r#"
val _ =
  case [3] of
    [] => 1
  | [_] => 2
  | [_, 7] => 3
  | [_, 5] => 4
  | [7, _] => 5
  | [_, _] => 6
  | x :: _ => x
"#,
  );
}

#[test]
fn list_unreachable() {
  check(
    r#"
val _ =
  case [3] of
    [] => 1
  | [_] => 2
  | [_, 7] => 3
  | [_, 5] => 4
  | [7, 7] => 5
(**  ^ unreachable pattern *)
  | [_, _] => 6
  | x :: _ => x
"#,
  );
}

#[test]
fn tuple_datatype_missing() {
  check(
    r#"
datatype ab = A | B
datatype cd = C | D
val x =
  case (A, C, A) of
(**    ^^^^^^^^^ non-exhaustive case: missing (A, D, B) *)
    (A, C, _) => 0
  | (B, _, _) => 1
  | (_, _, A) => 4
"#,
  );
}

#[test]
fn tuple_datatype_ok() {
  check(
    r#"
datatype ab = A | B
datatype cd = C | D
val x =
  case (A, C, A) of
    (A, C, A) => 0
  | (B, C, A) => 1
  | (_, _, B) => 3
  | (_, _, A) => 4
"#,
  );
}

#[test]
fn list_missing_nil() {
  check(
    r#"
fun f xs =
  case xs of
(**    ^^ non-exhaustive case: missing [] *)
    _ :: _ => 0
"#,
  );
}

#[test]
fn list_missing_cons() {
  check(
    r#"
fun f xs =
  case xs of
(**    ^^ non-exhaustive case: missing _ :: _ *)
    [] => 0
"#,
  );
}

#[test]
fn list_missing_len_1() {
  check(
    r#"
fun f xs =
  case xs of
(**    ^^ non-exhaustive case: missing [_] *)
    [] => 0
  | _ :: _ :: _ => 1
"#,
  );
}

#[test]
fn list_missing_len_3() {
  check(
    r#"
fun f xs =
  case xs of
(**    ^^ non-exhaustive case: missing [_, _, _] *)
    [] => 0
  | [_] => 1
  | [_, _] => 2
  | _ :: _ :: _ :: _ :: _ => 3
"#,
  );
}

#[test]
fn list_missing_true_singleton() {
  check(
    r#"
fun f xs =
  case xs of
(**    ^^ non-exhaustive case: missing [true] *)
    [] => 0
  | [false] => 1
  | _ :: _ :: _ => 2
"#,
  );
}

#[test]
fn list_missing_cons_prec() {
  check(
    r#"
fun append xs ys =
  case xs of
(**    ^^ non-exhaustive case: missing (_ :: _) :: _ *)
    [] => ys
  | [] :: ys => ys
"#,
  );
}

#[test]
fn undef_type_datatype() {
  check(
    r#"
datatype d = D of t
(**               ^ undefined type: t *)
fun f x =
  case x of
    D (_, _) => 1
"#,
  );
}

#[test]
fn undef_type_exn() {
  check(
    r#"
exception E of t
(**            ^ undefined type: t *)
val _ = 3 handle E (_, _) => 2
"#,
  );
}

#[test]
fn char() {
  check(
    r#"
exception NotDigit
val charToDigit = fn
    #"0" => 0
  | #"1" => 1
  | #"2" => 2
  | #"3" => 3
  | #"4" => 4
  | #"5" => 5
  | #"6" => 6
  | #"7" => 7
  | #"8" => 8
  | #"9" => 9
  | _ => raise NotDigit
"#,
  );
}

#[test]
fn record() {
  fail(
    r#"
val _ = fn {b=_, a=_} => 3
"#,
  );
}
