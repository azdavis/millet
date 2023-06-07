//! Literals.

use crate::check::check;

#[test]
fn char_big() {
  check(
    r#"
val _ = #"あ"
(**     ^^^^ character literal must have length 1 *)
"#,
  );
}

#[test]
fn char_small() {
  check(
    r#"
val _ = #""
(**     ^^^ character literal must have length 1 *)
"#,
  );
}

#[test]
fn int_big() {
  check(
    r#"
val _ = 123123123123123123123123132131
"#,
  );
}

#[test]
fn real_missing_digits() {
  check(
    r#"
val _ = 123.
(**     ^^^^ missing digits in number literal *)
"#,
  );
}

#[test]
fn string_continuation_non_whitespace() {
  check(
    r#"
val _ = "bad \ bad \ bad"
(**            ^ non-whitespace in string continuation *)
"#,
  );
}

#[test]
fn negative_word() {
  check(
    r#"
val _ = ~0w1
(**     ^^^^ negative word literal *)
"#,
  );
}

#[test]
fn string_unclosed() {
  check(
    r#"
val _ = "bad
(** + unclosed string literal *)
"#,
  );
}

#[test]
fn basic_escape() {
  check(
    r#"
val _: char   list = [#"\a", #"\b", #"\t", #"\n", #"\v", #"\f", #"\r", #"\\", #"\""]
val _: string list = [ "\a",  "\b",  "\t",  "\n",  "\v",  "\f",  "\r",  "\\",  "\""]
"#,
  );
}

#[test]
fn complex_escape() {
  check(
    r#"
val _: string list = [ "\^A",  "\^[",  "\^_",  "\123",  "\244",  "\u0000",  "\uBEEF",  "\uf00f"]
val _: char   list = [#"\^A", #"\^[", #"\^_", #"\123", #"\244", #"\u0000"] (* too long *)
"#,
  );
}
