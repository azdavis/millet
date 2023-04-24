//! Tests for the dynamics.

#![cfg(test)]
#![deny(clippy::pedantic, rust_2018_idioms)]

use std::io::BufRead as _;

#[allow(dead_code)]
fn check(s: &str) {
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
  let (mut syms, mut tys, bs) = sml_statics::basis::minimal();
  let mode = sml_statics_types::mode::Mode::Dynamics;
  let statics = sml_statics::get(&mut syms, &mut tys, &bs, mode, &sf.lower.arenas, &sf.lower.root);
  if let Some(e) = statics.errors.first() {
    panic!("statics error: {}", e.display(&syms, &tys, config::ErrorLines::One));
  }
  // TODO have these be the same as the ones from the std basis
  let match_ = syms.insert_exn(sml_path::Path::one(str_util::Name::new("Match")), None);
  let bind = syms.insert_exn(sml_path::Path::one(str_util::Name::new("Bind")), None);
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
  let mut s = sml_dynamics::Progress::Still;
  loop {
    println!("==>");
    println!("{dynamics}");
    stdin.read_line(&mut buf).expect("couldn't read");
    buf.clear();
    match s {
      sml_dynamics::Progress::Still => {}
      sml_dynamics::Progress::Val => {
        println!("stepped to a value");
        break;
      }
      sml_dynamics::Progress::Raise => {
        println!("raised an exception");
        break;
      }
    }
    s = dynamics.step();
  }
}

/*
#[test]
fn smoke() {
  check(
    r#"
datatype nat = Z | S of nat

fun add a b =
  case a of
    Z => b
  | S a => S (add a b)

val five = add (S (S Z)) (S (S (S Z)))
"#,
  );
}
 */