use crate::check::check;

#[test]
fn t_00() {
  check(
    r#"
val _ = 3 handle _ => 4
"#,
  );
}

#[test]
fn t_01() {
  check(
    r#"
exception Foo
and Bar of int
and Guh = Match
val _ =
  3 handle
    Foo => 1
  | Bar x => x
  | Match => 2
  | _ => 3
fun bar x = raise Bar x
val _ = 1 + bar 2 + (raise Foo) + 3
val _ = bar 3 andalso raise Guh
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn t_02() {
  check(
    r#"
(* we might want to raise an error here? *)
exception No = Match
val _ =
  3 handle
    Match => 1
  | No => 2
  | _ => 3
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn t_03() {
  check(
    r#"
val x = 3
exception Bad = x
(**             ^ mismatched identifier status: expected exception, found value *)
"#,
  );
}

#[test]
#[ignore = "todo for new"]
fn poly() {
  check(
    r#"
fun 'a foo (x: 'a) =
  let
    exception Poly of 'a
  in
    raise Poly x; raise Poly 3; ()
(**                     ^^^^^^ mismatched types: expected '22, found int *)
  end
"#,
  );
}

#[test]
fn raise_forall_a_a() {
  check(
    r#"
(* NOTE this may not be desirable *)
val x = raise Bind
val y = x x
"#,
  );
}
