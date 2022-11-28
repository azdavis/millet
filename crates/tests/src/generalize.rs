//! Tests targeted at the "generalize" step of the algorithm we use to implement HM.

use crate::check::check;

#[test]
fn no_over_generalize_infer_val() {
  check(
    r#"
fun id x =
  let val ret = x
  in ret end

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
fn no_over_generalize_infer_fun() {
  check(
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

#[test]
fn through_list() {
  check(
    r#"
exception E

fun go x =
  let val y = (fn [a] => a | _ => raise E) [x]
  in y end
"#,
  );
}

#[test]
fn recurse() {
  check(
    r#"
fun go n =
  if n <= 0 then ()
  else let val ret = go (n - 1) in ret end
"#,
  );
}
