use crate::check::check;

#[test]
fn many() {
  check(
    r#"
exception Uh
exception Foo
and Bar of int
and Guh = Uh
val _ =
  3 handle
    Foo => 1
  | Bar x => x
  | Uh => 2
  | _ => 3
fun bar x = raise Bar x
val _ = 1 + bar 2 + (raise Foo) + 3
val _ = bar 3 andalso raise Guh
"#,
  );
}

#[test]
fn alias_same() {
  check(
    r#"
exception A
exception B = A
val _ =
  3 handle
    B => 1
  | A => 2
(** ^ unreachable pattern *)
  | _ => 3
"#,
  );
}

#[test]
fn alias_not_exn() {
  check(
    r#"
val x = 3
    exception Bad = x
(** ^^^^^^^^^^^^^^^^^ not an exception: x *)
"#,
  );
}

#[test]
fn poly() {
  check(
    r#"
fun 'a foo (x: 'a) =
  let
    exception Poly of 'a
  in
    raise Poly x; raise Poly 3; ()
(**                          ^ expected 'a, found int *)
  end
"#,
  );
}

#[test]
fn raise_forall_a_a() {
  check(
    r#"
exception E
val x = raise E
(**     ^^^^^^^ cannot bind expansive polymorphic expression *)
val y = x x
"#,
  );
}
