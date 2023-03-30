//! `local`.

use crate::check::check;

#[test]
fn use_later_dec() {
  check(
    r#"
val _ =
  let
    local
      val x = 3
    in
      val y = x + 2
      val z = y + x
    end
  in
    y + z
  end
"#,
  );
}

#[test]
fn does_not_escape_dec() {
  check(
    r#"
val _ =
  let
    local
      val x = 3
    in
    end
  in
    x
(** ^ undefined value: `x` *)
  end
"#,
  );
}

#[test]
fn use_later_str_dec() {
  check(
    r#"
local
  val x = 3
in
  val y = x + 2
  val z = y + x
end
val _ = y + z
"#,
  );
}

#[test]
fn does_not_escape_str_dec() {
  check(
    r#"
local
  val x = 3
in
end
val _ = x
(**     ^ undefined value: `x` *)
"#,
  );
}
