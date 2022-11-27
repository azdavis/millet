use crate::check::check;

#[test]
fn scon() {
  check(
    r#"
val _: int = 3
and _: int = ~4
and _: real = 3.2
and _: real = ~3.2e9
and _: real = 3.2E9
and _: real = 3.2E~9
and _: word = 0w123
and _: int = 0x123beef
and _: word = 0wx123beef
and _: char = #"a"
and _: string = "foo"
(* not actually scon *)
and _: bool = true
and _: bool = false
"#,
  );
}

#[test]
fn record() {
  check(
    r#"
val _ = {}
val _ = { x = 4 }
val _ = { a = 3, b = "hi" }
"#,
  );
}

#[test]
fn tuple() {
  check(
    r#"
val _ = ()
(* not actually a tuple *)
val _ = (3)
val _ = (1, "hi")
"#,
  );
}

#[test]
fn list() {
  check(
    r#"
val _: int list = []
val _: int list = [1]
val _: int list = [2, 3]
"#,
  );
}

#[test]
fn seq() {
  check(
    r#"
val _: int = ("no"; false; 4)
"#,
  );
}

#[test]
fn let_() {
  check(
    r#"
val _: int = let val x = 3 in x end
"#,
  );
}

#[test]
fn app_fn() {
  check(
    r#"
val uh = fn _ => 3
val _: int = uh ()
val _: int = uh 3
val _: int = uh "hi"
val _: int = uh {}
"#,
  );
}

#[test]
fn infix_fn() {
  check(
    r#"
val uh = fn _ => 3
infix uh
val _: int = 3 uh 4
val _: int = () uh ()
"#,
  );
}

#[test]
fn typed() {
  check(
    r#"
val _ = 3: int
val _ = "hi": string
"#,
  );
}

#[test]
fn andalso() {
  check(
    r#"
val _ = false andalso true
val _ = fn (a, b, c) => a andalso b andalso c
"#,
  );
}

#[test]
fn orelse() {
  check(
    r#"
val _ = false orelse true
val _ = fn (a, b, c) => a orelse b orelse c
"#,
  );
}

#[test]
fn handle() {
  check(
    r#"
val _ = 3 handle Match => 4
"#,
  );
}

#[test]
fn raise() {
  check(
    r#"
exception E
val _ = fn () => raise E
"#,
  );
}

#[test]
fn if_() {
  check(
    r#"
val _ = fn (a, b, c) => if a then b 1 else c 2
val _ = if 1 then 2 else 3
(**        ^ contains: expected bool, found int *)
"#,
  );
}

#[test]
fn while_() {
  check(
    r#"
val _ = fn (a, b) => while a () do b ()
"#,
  );
}

#[test]
fn case() {
  check(
    r#"
val _ = fn x => case x of 1 => 1 | _ => 2
"#,
  );
}

#[test]
fn l_round_exp_tail() {
  check(
    r#"
val x = (3 val
(**        ^^^ expected `)`, `,`, or `;` *)
"#,
  );
}

#[test]
fn ref_() {
  check(
    r#"
val x = ref 3
val _: int =
  case x of
    ref y => y
val ref z = x
val _: int = z
"#,
  );
}

#[test]
fn string_with_comment() {
  check(
    r#"
val _: string = "(* this looks like a comment! *)"
val _: int = 4
"#,
  );
}

#[test]
fn hole() {
  check(
    r#"
val _ = if _ then "yes" else "no"
(**        ^ expression hole with type bool *)
"#,
  );
}
