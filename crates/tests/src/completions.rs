//! Tests for completions.

use crate::check::{check, raw};

#[test]
fn smoke() {
  check(
    r#"
val foo = 3

(**     vv undefined *)
val _ = aa
(**     ^ completions(with-std): foo *)
"#,
  );
}

#[test]
fn in_struct() {
  check(
    r#"
structure Foo = struct
  val bar = 3
  val quz = "hi"
end

(**          vvv expected a name *)
val _ = Foo. val
(**         ^ completions: bar, quz *)
"#,
  );
}

#[test]
fn nested() {
  check(
    r#"
structure A = struct
  val x = 3
  structure B = struct
    val y = 4
  end
end

(**          vvv expected a name *)
val _ = A.B. val
(**         ^ completions: y *)
"#,
  );
}

#[test]
fn eof() {
  let sml = r#"
val a = 1 and b = 2 and c = 3
(**     v completions(with-std): a, b, c *)
val _ = x"#;
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Minimal,
    outcome: raw::Outcome::Pass,
    limit: raw::Limit::All,
    min_severity: diagnostic::Severity::Error,
    expected_input: raw::ExpectedInput::Good,
  };
  raw::get(raw::one_file_fs(sml), opts);
}

#[test]
fn in_path() {
  // the completions should be auto-narrowed to just `foo` by the editor
  check(
    r#"
structure S = struct
  val foo = ()
  val bar = ()
end

(**     vvv undefined value: `f` *)
val _ = S.f
(**       ^ completions: foo, bar *)
"#,
  );
}
