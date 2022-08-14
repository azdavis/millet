use crate::check::{check, fail};

#[test]
fn no_over_generalize_infer() {
  fail(
    r#"
fun id x =
  let fun get () = x
  in get () end

val y = id ()
val _ = y
(**     ^ hover: unit *)
val z = id 3
val _ = z
(**     ^ hover: int *)
"#,
  );
}

#[test]
fn no_over_generalize_fixed() {
  check(
    r#"
fun 'a id (x : 'a) =
  let fun get () = x
  in get () end

val y = id ()
val _ = y
(**     ^ hover: unit *)
val z = id 3
val _ = z
(**     ^ hover: int *)
"#,
  );
}
