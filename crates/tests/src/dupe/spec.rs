use crate::check::check;

#[test]
fn val() {
  check(
    r#"
signature S = sig
  val x: int val x: int
(**          ^^^^^^^^^^ duplicate value: x *)
end
"#,
  );
}

#[test]
fn ty() {
  check(
    r#"
signature S = sig
  type t type t
(**      ^^^^^^ duplicate type: t *)
end
"#,
  );
}

#[test]
fn eq_ty() {
  check(
    r#"
signature S = sig
  eqtype t eqtype t
(**        ^^^^^^^^ duplicate type: t *)
end
"#,
  );
}

#[test]
fn datatype() {
  check(
    r#"
signature S = sig
  datatype t = A datatype t = B
(**              ^^^^^^^^^^^^^^ duplicate type: t *)
end
"#,
  );
}

#[test]
fn exception() {
  check(
    r#"
signature S = sig
  exception E exception E
(**           ^^^^^^^^^^^ duplicate value: E *)
end
"#,
  );
}

#[test]
fn structure() {
  check(
    r#"
signature S = sig
  structure A : sig end structure A : sig end
(**                     ^^^^^^^^^^^^^^^^^^^^^ duplicate structure: A *)
end
"#,
  );
}
