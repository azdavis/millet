use crate::check::check;

#[test]
fn t_00() {
  check(
    r#"
val _ =
  let datatype bad = guh
  in guh end
(**  ^^^ type name escapes its scope: bad *)
"#,
  );
}

#[test]
fn t_01() {
  check(
    r#"
val _ =
  let
    datatype foo = bar
    val quz = bar
  in
    (true; false; bar; 3 + 3; quz)
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ type name escapes its scope: foo *)
  end
"#,
  );
}

#[test]
fn t_02() {
  check(
    r#"
val _ =
  let
    datatype guh = bad
  in
    if 3 < 4 then [] else [(3, bad, false, "hey")]
(** ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ type name escapes its scope: guh *)
  end
"#,
  );
}

#[test]
fn t_03() {
  check(
    r#"
datatype t = One
val _ = let datatype t = Two in Two end
(**                             ^^^ type name escapes its scope: t *)
"#,
  );
}

#[test]
fn t_04() {
  check(
    r#"
datatype t = One
val _ = let datatype t = Two in One end
"#,
  );
}
