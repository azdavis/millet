//! Tests for MLB syntax.

fn check(s: &str) {
  mlb_syntax::get(s, &slash_var_path::Env::default()).unwrap();
}

#[test]
fn smoke_ok() {
  check(
    r#"
basis A = let in B end and C = let foo.sml in D end
basis Uh = bas uh.sml end
open A C
(* hello there *)
local
  foo.sml
  bar.sml
  quz.mlb
  uh.fun
in
  structure E
  signature F = G
  functor H and I = J
end
ann "huh" in huh.sml end
signature ARRAY_UTIL
"#,
  );
}
