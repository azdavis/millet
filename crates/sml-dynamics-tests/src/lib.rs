//! Tests for the dynamics.

#![cfg(test)]
#![deny(clippy::pedantic, rust_2018_idioms)]
#![allow(clippy::too_many_lines)]

use std::io::BufRead as _;

#[allow(dead_code)]
fn check(s: &str, steps: &[&str]) {
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
    panic!("statics error: {}", e.display(&syms_tys, config::ErrorLines::One));
  }
  // TODO have these be the same as the ones from the std basis
  let match_ = syms_tys.syms.insert_exn(sml_path::Path::one(str_util::Name::new("Match")), None);
  let bind = syms_tys.syms.insert_exn(sml_path::Path::one(str_util::Name::new("Bind")), None);
  let mut decs = Vec::<sml_hir::DecIdx>::new();
  for &str_dec in &sf.lower.root {
    match &sf.lower.arenas.str_dec[str_dec] {
      sml_hir::StrDec::Dec(ds) => decs.extend(ds.iter().copied()),
      sd => panic!("unsupported str dec for dynamics: {sd:?}"),
    }
  }
  let cx = sml_dynamics::Cx {
    ars: &sf.lower.arenas,
    exp: &statics.exp_id_statuses,
    pat: &statics.pat_id_statuses,
    bind,
    match_,
  };
  let mut dynamics = sml_dynamics::Dynamics::new(cx, decs).expect("no decs");
  let mut stdin = std::io::stdin().lock();
  let mut buf = String::new();
  let debug = std::env::var_os("MILLET_DEBUG").map_or(false, |x| x == "1");
  let use_steps = std::env::var_os("MILLET_STEPS").map_or(false, |x| x == "1");
  let mut steps = steps.iter();
  loop {
    if debug {
      println!("==>");
      dynamics.show_debug();
      println!("{dynamics:#}");
      stdin.read_line(&mut buf).expect("couldn't read");
      buf.clear();
    }
    if use_steps {
      let want = rm_whitespace(steps.next().expect("missing step").trim());
      let got = rm_whitespace(&dynamics.to_string());
      pretty_assertions::assert_str_eq!(want, got);
    }
    match dynamics.step() {
      sml_dynamics::Progress::Still(d) => dynamics = d,
      sml_dynamics::Progress::Done => {
        if debug {
          println!("done");
        }
        break;
      }
      sml_dynamics::Progress::Raise => {
        if debug {
          println!("raised an exception");
        }
        break;
      }
    }
  }
  if use_steps {
    assert!(steps.next().is_none());
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
    &[],
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
  | S a => S (add a b)

val five = add (S (S Z)) (S (S (S Z)))
"#,
    &[
      r#"
datatype ...

val rec add = fn '0 => fn '1 =>
  (fn (a, b) =>
    (fn
      Z => b
    | S a => S (add a b)
    ) a
  ) ('0, '1)

val five = add
  (S (S Z))
  (S (S (S Z)))
"#,
      r#"
val five =
  (
    fn '0 => fn '1 =>
    (fn (a, b) =>
      (fn
        Z => b
      | S a => S (add a b)
      ) a
    ) ('0, '1)
  )
  (S (S Z))
  (S (S (S Z)))
"#,
      // lazy substitution
      r#"
val five =
  (
    fn '1 =>
    (fn (a, b) =>
      (fn
        Z => b
      | S a => S (add a b)
      ) a
    ) ('0, '1)
  )
  (S (S (S Z)))
"#,
      r#"
val five =
  (fn (a, b) =>
    (fn
      Z => b
    | S a => S (add a b)
    ) a
  ) ('0, '1)
"#,
      r#"
val five =
  (fn (a, b) =>
    (fn
      Z => b
    | S a => S (add a b)
    ) a
  ) (S (S Z), '1)
"#,
      r#"
val five =
  (fn (a, b) =>
    (fn
      Z => b
    | S a => S (add a b)
    ) a
  ) (S (S Z), S (S (S Z)))
"#,
      r#"
val five =
  (fn
    Z => b
  | S a => S (add a b)
  ) a
"#,
      // TODO
      r#"
val five =
  (fn
    Z => b
  | S a => S (add a b)
  ) (S (S Z))
"#,
      r#"
val five =
  S (add a b)
"#,
      r#"
val five =
  S (add (S Z) b)
"#,
      r#"
val five =
  S (add (S Z) (S (S (S Z))))
"#,
    ],
  );
}
