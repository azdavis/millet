//! Tests for `open`.

use crate::check::check_with_warnings;

#[test]
fn smoke_err() {
  check_with_warnings(
    r#"
structure A = struct end
open A
(** + top-level `open` *)
"#,
  );
}

#[test]
fn local_ok() {
  check_with_warnings(
    r#"
structure A = struct end
local open A in end
"#,
  );
}

#[test]
fn in_err() {
  check_with_warnings(
    r#"
structure A = struct end
local in open A end
(**      ^^^^^^ top-level `open` *)
"#,
  );
}

#[test]
fn nested_local_ok() {
  check_with_warnings(
    r#"
structure A = struct end
local
  local
    open A
  in
    open A
  end
in
end
"#,
  );
}

#[test]
fn nested_in_ok() {
  check_with_warnings(
    r#"
structure A = struct end
local
in
  local
    open A
  in
  end
end
"#,
  );
}

#[test]
fn nested_in_err() {
  check_with_warnings(
    r#"
structure A = struct end
local
in
  local
  in
    open A
(** ^^^^^^ top-level `open` *)
  end
end
"#,
  );
}

#[test]
fn let_ok() {
  check_with_warnings(
    r#"
structure A = struct end
val s = let open A in "hi" end
"#,
  );
}

#[test]
fn structure_ok() {
  check_with_warnings(
    r#"
structure A = struct end
structure B = struct open A end
"#,
  );
}

#[test]
fn functor_ok() {
  check_with_warnings(
    r#"
structure A = struct end
functor F() = struct open A end
"#,
  );
}
