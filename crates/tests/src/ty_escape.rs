//! Type names escaping the scope in which they are valid.

use crate::check::check;

#[test]
fn smoke() {
  check(
    r#"
val _ = let datatype bad = guh in guh end
(**     ^^^ type escapes its scope: `bad` *)
"#,
  );
}

#[test]
fn seq() {
  check(
    r#"
val _ = let datatype foo = bar val quz = bar in (bar; 3 + 3; quz) end
(**     ^^^ type escapes its scope: `foo` *)
"#,
  );
}

#[test]
fn does_not_escape() {
  check(
    r#"
val _ = let datatype foo = bar val quz = bar in (bar; 3 + 3; quz; 123) end
"#,
  );
}

#[test]
fn branch() {
  check(
    r#"
val _ = let datatype d = D in if 3 < 4 then [] else [(3, D, "hi")] end
(**     ^^^ type escapes its scope: `d` *)
"#,
  );
}

#[test]
fn shadow() {
  check(
    r#"
datatype t = One
val _ = let datatype t = Two in Two end
(**     ^^^ type escapes its scope: `t` *)
"#,
  );
}

#[test]
fn ok() {
  check(
    r#"
datatype t = One
val _ = let datatype t = Two in One end
"#,
  );
}

#[test]
fn exn_ctor() {
  check(
    r#"
val ex =
  let
    exception E
  in
    E
  end
"#,
  );
}

#[test]
fn raise_datatype() {
  check(
    r#"
val _ =
  let
    datatype d = D
    exception E of d
  in
    (* cannot be specifically matched against *)
    fn () => raise E D
  end
"#,
  );
}
