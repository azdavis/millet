//! Vector expressions and patterns like `#[a, b]`.

use crate::check::check;

#[test]
fn exp_default_disallow() {
  check(
    r#"
val _ = #[1, 2]
(**     ^^^^^^^ vector expressions *)
"#,
  );
}

#[test]
fn pat_default_disallow() {
  check(
    r#"
val _ = fn #[x, 2] => x | _ => 2
(**        ^^^^^^^ vector patterns *)
"#,
  );
}
