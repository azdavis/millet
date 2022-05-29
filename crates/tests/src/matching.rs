use crate::check::check;

#[test]
fn t_00() {
  check(
    r#"
val _ =
  case 3 of
(**    ^ non-exhaustive match *)
    4 => 5
"#,
  );
}

#[test]
fn t_01() {
  check(
    r#"
datatype hmm = A | B of int | C of hmm | D of string
val _ =
  case A of
(**    ^ non-exhaustive match *)
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
fn t_02() {
  check(
    r#"
val _ =
  case (1, 2) of
(**    ^^^^^^ non-exhaustive match *)
    (3, 4) => 0
  | (x, 6) => x
"#,
  );
}

#[test]
fn t_03() {
  check(
    r#"
val _ =
  case (3, "hey", false, LESS) of
    (3, "guh", _, _) => 0
  | (_, "hey", false, GREATER) => 1
  | (4, _, false, _) => 2
  | (_, _, false, EQUAL) => 3
  | (_, _, false, _) => 4
  | (3, _, true, LESS) => 5
  | (_, "nope", true, LESS) => 6
  | (n, _, true, LESS) => n
  | (_, _, true, EQUAL) => 8
  | (_, _, true, GREATER) => 9
"#,
  );
}

#[test]
fn t_04() {
  check(
    r#"
val _ =
  case (true, false) of
(**    ^^^^^^^^^^^^^ non-exhaustive match *)
    (true, _) => 0
  | (_, false) => 1
"#,
  );
}

#[test]
fn t_05() {
  check(
    r#"
val _ =
  case (1, 3.3, "hey", LESS, false) of
    (1, _, _, LESS, _) => 0
  | (3, x, "foo", GREATER, true) => 1
  | (_, _, "nope", EQUAL, _) => 2
  | (_, _, "guy", EQUAL, _) => 3
  | (_, _, "thing", EQUAL, _) => 4
  | (_, _, _, EQUAL, true) => 5
  | (x, _, _, EQUAL, false) => x
  | (_, _, _, LESS, true) => 5
  | (x, _, _, LESS, false) => x
  | (_, _, _, GREATER, true) => 5
  | (x, _, _, GREATER, false) => x
"#,
  );
}

#[test]
fn t_06() {
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
fn t_07() {
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
fn t_08() {
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
fn t_09() {
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
fn t_10() {
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
fn t_11() {
  check(
    r#"
datatype ab = A | B
datatype cd = C | D
val x =
  case (A, C, A) of
(**    ^^^^^^^^^ non-exhaustive match *)
    (A, C, _) => 0
  | (B, _, _) => 1
  | (_, _, A) => 4
"#,
  );
}

#[test]
fn t_12() {
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
