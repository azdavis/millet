use crate::check::check;

#[test]
fn t_01() {
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
fn t_02() {
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
(** ^ undefined value: x *)
  end
"#,
  );
}

#[test]
fn t_03() {
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
fn t_04() {
  check(
    r#"
local
  val x = 3
in
end
val _ = x
(**     ^ undefined value: x *)
"#,
  );
}
