//! Big expectation, big expectation. Ooh, you and me, we got big expectations.

use crate::check::check_multi;

#[test]
fn fst() {
  let files = [
    (
      "millet.toml",
      r#"
version = 1
[workspace]
root = "*/sources.cm"
[workspace.path-vars]
hashida = { value = "itaru" }
okabe = { path = "rintarou" }
shiina = { workspace-path = "mayuri" }
"#,
    ),
    (
      "rintarou.sml",
      r#"
structure R = struct
  val date = 20100728
end
"#,
    ),
    (
      "hw1/mayuri.sml",
      r#"
structure M = struct
  val idx = 2
end
"#,
    ),
    (
      "hw1/sources.cm",
      r#"
Group is
  a.sml
  b.sml
  $shiina.sml
  $okabe.sml
"#,
    ),
    (
      "hw1/a.sml",
      r#"
structure A = struct
  val bird = "polly"
end
"#,
    ),
    (
      "hw1/b.sml",
      r#"
val _ = A.bird

val _ = M.idx + 1 + R.date
"#,
    ),
    (
      "hw2/mayuri.sml",
      r#"
structure M = struct
  val msg = "tut-tu-ru!"
end
"#,
    ),
    (
      "hw2/sources.cm",
      r#"
Group is
  c.sml
  $shiina.sml
  $okabe.sml
  d.sml
"#,
    ),
    (
      "hw2/d.sml",
      r#"
structure D = struct
  val sound = "beep"
end

val _ = (if M.msg = "E" then 1 else 2) + R.date
"#,
    ),
    (
      "hw2/c.sml",
      r#"
val _ = D.sound
"#,
    ),
  ];
  check_multi(files);
}
