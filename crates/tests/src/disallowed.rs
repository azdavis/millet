//! Disallowing parts of the language.

use crate::check::check_multi;

#[test]
fn exp() {
  let config = r#"
version = 1
language.exp.while = false
"#;
  let mlb = r#"
a.sml
"#;
  let sml = r#"
val _ = 4
val _ = while true do ()
(**     ^^^^^^^^^^^^^^^^ disallowed expression: while *)
val _ = "hi"
"#;
  check_multi([(config::file::NAME, config), ("s.mlb", mlb), ("a.sml", sml)]);
}
