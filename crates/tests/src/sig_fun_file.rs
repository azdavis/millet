//! Special file kinds which only allow certain kinds of declarations.

use crate::check::raw;

const OPTS: raw::Opts<'_> = raw::Opts {
  std_basis: raw::StdBasis::Minimal,
  outcome: raw::Outcome::Pass,
  limit: raw::Limit::First,
  min_severity: diagnostic::Severity::Warning,
  expected_input: raw::ExpectedInput::Good,
};

const CONFIG: &str = r#"
version = 1
language.successor-ml.do-dec = true
"#;

#[track_caller]
fn check_sig(s: &str) {
  raw::get([(config::file::PATH, CONFIG), ("a.mlb", "a.sig"), ("a.sig", s)], OPTS);
}

#[track_caller]
fn check_fun(s: &str) {
  raw::get([(config::file::PATH, CONFIG), ("a.mlb", "a.fun"), ("a.fun", s)], OPTS);
}

#[test]
fn sig_zero() {
  // TODO this should fail, since it contains 0 (not 1)
  check_sig("");
}

#[test]
fn fun_zero() {
  // TODO this should fail, since it contains 0 (not 1)
  check_fun("");
}

#[test]
fn sig_one() {
  check_sig(
    r#"
signature S = sig
  type t
  val x: t
  structure S: sig
    type a
    val z: a
  end
end
"#,
  );
}

#[test]
fn fun_one() {
  check_fun(
    r#"
functor F() = struct
  val x = 3
  fun f() = ()
  type a = int
  datatype b = D
  datatype c = datatype int
  exception E
  nonfix f
  do f()
  local val z = 3 in val a = z + 1 end
  structure S = struct val a = x end
end
"#,
  );
}

#[test]
fn sig_two() {
  check_sig(
    r#"
signature S1 = sig end
signature S2 = sig end
(** + files usually contain exactly one *)
"#,
  );
}

#[test]
fn fun_two() {
  check_fun(
    r#"
functor F1() = struct end
functor F2() = struct end
(** + files usually contain exactly one *)
"#,
  );
}

#[test]
fn sig_in_fun() {
  check_fun(
    r#"
signature S = sig end
(** + files usually contain exactly one *)
"#,
  );
}

#[test]
fn fun_in_sig() {
  check_sig(
    r#"
functor F() = struct end
(** + files usually contain exactly one *)
"#,
  );
}

#[test]
fn many_dec_kinds() {
  let decs = [
    "val x = 3",
    "fun f() = 3",
    "type t = int",
    "datatype t = D",
    "datatype t = datatype int",
    "exception E",
    "infix +",
    "do ((fn () => ()) ())",
    "structure S = struct end",
    "local val x = 3 in signature S = sig end end",
    "local val x = 3 in functor F() = struct end end",
  ];
  for dec in decs {
    let mut contents = dec.to_owned();
    contents.push('\n');
    contents.push_str("(** + files usually contain exactly one *)");
    check_fun(&contents);
    check_sig(&contents);
  }
}
