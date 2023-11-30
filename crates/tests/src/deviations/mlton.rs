//! See [`MLton` docs](http://mlton.org/UnresolvedBugs) about unresolved bugs.

use crate::check::check;

#[test]
fn fun_bar_case() {
  // NOTE: the specific error doesn't matter a whole lot, this just illustrates that we (along with
  // every sml impl) require `()` to disambiguate this case (literally).
  check(
    r"
fun f 0 y =
    case x of
      1 => 2
    | _ => 3
  | f _ y = 4
(**     ^ non-infix name used as infix *)
",
  );
}

#[test]
fn rebind_ctor() {
  check(
    r"
datatype uh = Uh
val rec Uh = fn () => ()
",
  );
  cov_mark::check("rebind_ctor");
}

#[test]
fn functor_re_typecheck_or_not_1() {
  check(
    r#"
fun id x = x
functor F (X: sig type t end) = struct
  val f = id id
(**       ^^^^^ cannot bind expansive polymorphic expression *)
end
structure A = F (struct type t = int end)
structure B = F (struct type t = bool end)
val _ = A.f 10
val _ = B.f "dude"
"#,
  );
}

#[test]
fn functor_re_typecheck_or_not_2() {
  check(
    r"
fun id x = x
functor F (X: sig type t end) = struct
  val f = id id
(**       ^^^^^ cannot bind expansive polymorphic expression *)
end
structure A = F (struct type t = int end)
structure B = F (struct type t = bool end)
val _ = A.f 10
val _ = B.f false
",
  );
}
