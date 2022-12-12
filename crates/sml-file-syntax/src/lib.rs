//! See [`SourceFileSyntax`].

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

/// A source file analyzed at the purely syntactic level.
#[derive(Debug)]
pub struct SourceFileSyntax {
  /// The position database for this file.
  pub pos_db: text_pos::PositionDb,
  /// Lex errors from the file.
  pub lex_errors: Vec<sml_lex::Error>,
  /// The lossless concrete syntax tree.
  pub parse: sml_parse::Parse,
  /// The lowered HIR.
  pub lower: sml_lower::Lower,
}

impl SourceFileSyntax {
  /// Starts processing a single source file.
  pub fn new(fix_env: &mut sml_fixity::Env, contents: &str) -> Self {
    elapsed::log("SourceFileSyntax::new", || {
      let (lex_errors, parse) = Self::lex_and_parse(fix_env, contents);
      let mut lower = sml_lower::get(&parse.root);
      sml_ty_var_scope::get(&mut lower.arenas, lower.root);
      Self { pos_db: text_pos::PositionDb::new(contents), lex_errors, parse, lower }
    })
  }

  /// Lex and parse a source file.
  pub fn lex_and_parse(
    fix_env: &mut sml_fixity::Env,
    contents: &str,
  ) -> (Vec<sml_lex::Error>, sml_parse::Parse) {
    let lexed = sml_lex::get(contents);
    let parse = sml_parse::get(&lexed.tokens, fix_env);
    (lexed.errors, parse)
  }
}
