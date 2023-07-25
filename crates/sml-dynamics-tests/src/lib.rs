//! Tests for the dynamics.

#![cfg(test)]
#![deny(clippy::pedantic, rust_2018_idioms)]
#![allow(clippy::too_many_lines)]

use std::io::BufRead as _;

fn env_var_enabled(s: &str) -> bool {
  std::env::var_os(s).map_or(false, |x| x == "1")
}

#[allow(dead_code)]
fn check(s: &str, steps: &[&str]) {
  let show_debug = env_var_enabled("MILLET_SHOW_DEBUG");
  let manually_advance = env_var_enabled("MILLET_MANUALLY_ADVANCE");
  let check_steps = !env_var_enabled("MILLET_NO_CHECK_STEPS");
  let mut fix_env = sml_fixity::STD_BASIS.clone();
  let lang = config::lang::Language::default();
  let sf = sml_file_syntax::SourceFileSyntax::new(&mut fix_env, &lang, s);
  if let Some(e) = sf.lex_errors.first() {
    panic!("lex error: {e}");
  }
  if let Some(e) = sf.parse.errors.first() {
    panic!("parse error: {e}");
  }
  if let Some(e) = sf.lower.errors.first() {
    panic!("lower error: {e}");
  }
  let (mut syms_tys, bs) = sml_statics::basis::minimal();
  let mode = sml_statics_types::mode::Mode::Dynamics;
  let statics = sml_statics::get(&mut syms_tys, &bs, mode, &sf.lower.arenas, &sf.lower.root);
  if let Some(e) = statics.errors.first() {
    panic!("statics error: {}", e.display(&syms_tys, config::DiagnosticLines::One));
  }
  // TODO have these be the same as the ones from the std basis
  let match_ = syms_tys.syms.insert_exn(sml_path::Path::one(str_util::Name::new("Match")), None);
  let bind = syms_tys.syms.insert_exn(sml_path::Path::one(str_util::Name::new("Bind")), None);
  let cx = sml_dynamics::Cx {
    ars: &sf.lower.arenas,
    exp: &statics.exp_id_statuses,
    pat: &statics.pat_id_statuses,
    bind,
    match_,
  };
  let mut dynamics = sml_dynamics::Dynamics::new(cx, sf.lower.root.clone()).expect("no str decs");
  let mut stdin = std::io::stdin().lock();
  let mut buf = String::new();
  let mut steps = steps.iter();
  loop {
    if show_debug {
      println!("==>");
      dynamics.show_debug();
      println!("{dynamics:#}");
    }
    if manually_advance {
      stdin.read_line(&mut buf).expect("couldn't read");
      buf.clear();
    }
    if check_steps {
      let want = rm_whitespace(steps.next().expect("missing step").trim());
      let got = rm_whitespace(&dynamics.to_string());
      pretty_assertions::assert_str_eq!(want, got);
    }
    match dynamics.step() {
      sml_dynamics::Progress::Still(d) => dynamics = d,
      sml_dynamics::Progress::Done => {
        if show_debug {
          println!("done");
        }
        break;
      }
      sml_dynamics::Progress::Raise => {
        if show_debug {
          println!("raised an exception");
        }
        break;
      }
    }
  }
  if check_steps {
    if let Some(step) = steps.next() {
      panic!("extra step: {step}");
    }
  }
}

fn rm_whitespace(s: &str) -> String {
  s.replace(char::is_whitespace, "")
}

#[test]
fn builtin_add() {
  check(
    r#"
val inc = fn x => 1 + x
val three = inc 2
"#,
    &[
      r#"
val inc = fn x => + (1, x)
val three = inc 2
"#,
      r#"
val three = inc 2
"#,
      r#"
val three = (fn x => + (1, x)) 2
"#,
      r#"
val three = + (1, x)
"#,
      r#"
val three = + (1, 2)
"#,
      r#"
val three = 3
"#,
    ],
  );
}

#[test]
fn nat_add() {
  check(
    r#"
datatype nat = Z | S of nat

fun add a b =
  case a of
    Z => b
  | S x => S (add x b)

val four = add (S Z) (S (S (S Z)))
"#,
    &[
      r#"
datatype ...

val rec add = fn '0 => fn '1 =>
  (fn (a, b) =>
    (fn
      Z => b
    | S x => S (add x b)
    ) a
  ) ('0, '1)

val four = add
  (S Z)
  (S (S (S Z)))
"#,
      r#"
val rec add = fn '0 => fn '1 =>
  (fn (a, b) =>
    (fn
      Z => b
    | S x => S (add x b)
    ) a
  ) ('0, '1)

val four = add
  (S Z)
  (S (S (S Z)))
"#,
      r#"
val four = add
  (S Z)
  (S (S (S Z)))
"#,
      r#"
val four =
  (
    fn '0 => fn '1 =>
    (fn (a, b) =>
      (fn
        Z => b
      | S x => S (add x b)
      ) a
    ) ('0, '1)
  )
  (S Z)
  (S (S (S Z)))
"#,
      // lazy substitution
      r#"
val four =
  (
    fn '1 =>
    (fn (a, b) =>
      (fn
        Z => b
      | S x => S (add x b)
      ) a
    ) ('0, '1)
  )
  (S (S (S Z)))
"#,
      r#"
val four =
  (fn (a, b) =>
    (fn
      Z => b
    | S x => S (add x b)
    ) a
  ) ('0, '1)
"#,
      r#"
val four =
  (fn (a, b) =>
    (fn
      Z => b
    | S x => S (add x b)
    ) a
  ) (S Z, '1)
"#,
      r#"
val four =
  (fn (a, b) =>
    (fn
      Z => b
    | S x => S (add x b)
    ) a
  ) (S Z, S (S (S Z)))
"#,
      r#"
val four =
  (fn
    Z => b
  | S x => S (add x b)
  ) a
"#,
      r#"
val four =
  (fn
    Z => b
  | S x => S (add x b)
  ) (S Z)
"#,
      r#"
val four =
  S (add x b)
"#,
      r#"
val four =
  S (
      (
        fn '0 => fn '1 =>
        (fn (a, b) =>
          (fn
            Z => b
          | S x => S (add x b)
          ) a
      ) ('0, '1)
    )
    x
    b
  )
"#,
      r#"
val four =
  S (
      (
        fn '0 => fn '1 =>
        (fn (a, b) =>
          (fn
            Z => b
          | S x => S (add x b)
          ) a
      ) ('0, '1)
    )
    Z
    b
  )
"#,
      r#"
val four =
  S (
      (
        fn '1 =>
        (fn (a, b) =>
          (fn
            Z => b
          | S x => S (add x b)
          ) a
      ) ('0, '1)
    )
    b
  )
"#,
      r#"
val four =
  S (
      (
        fn '1 =>
        (fn (a, b) =>
          (fn
            Z => b
          | S x => S (add x b)
          ) a
      ) ('0, '1)
    )
    (S (S (S Z)))
  )
"#,
      r#"
val four =
  S (
    (fn (a, b) =>
      (fn
        Z => b
      | S x => S (add x b)
      ) a
    ) ('0, '1)
  )
"#,
      r#"
val four =
  S (
    (fn (a, b) =>
      (fn
        Z => b
      | S x => S (add x b)
      ) a
    ) (Z, '1)
  )
"#,
      r#"
val four =
  S (
    (fn (a, b) =>
      (fn
        Z => b
      | S x => S (add x b)
      ) a
    ) (Z, S (S (S Z)))
  )
"#,
      r#"
val four =
  S (
    (fn
      Z => b
    | S x => S (add x b)
    ) a
  )
"#,
      r#"
val four =
  S (
    (fn
      Z => b
    | S x => S (add x b)
    ) Z
  )
"#,
      r#"
val four =
  S b
"#,
      r#"
val four =
  S (S (S (S Z)))
"#,
    ],
  );
}
