//! Generating Rust code from the ungrammar.

use fast_hash::FxHashMap;
use identifier_case::snake_to_pascal;
use syntax_gen::{gen, Token, TokenKind};

const SPECIAL: [(&str, &str); 7] = [
  ("Name", "a name"),
  ("TyVar", "a type variable"),
  ("IntLit", "an integer literal"),
  ("RealLit", "a real literal"),
  ("WordLit", "a word literal"),
  ("CharLit", "a character literal"),
  ("StringLit", "a string literal"),
];

const TOKENS: &str = include_str!("../../docs/tokens.md");

fn code_h2(s: &str) -> Option<&str> {
  s.strip_prefix("## `")?.strip_suffix('`')
}

fn main() -> std::io::Result<()> {
  let mut doc_map = FxHashMap::<&str, String>::default();
  let mut s = String::new();
  let mut tok = None::<&str>;
  for line in TOKENS.lines() {
    match code_h2(line) {
      Some(new_tok) => {
        if let Some(tok) = tok {
          assert!(doc_map.insert(tok, s).is_none());
        }
        tok = Some(new_tok);
        // TODO this used to be a sml code block, but the VS Code markdown viewer doesn't do well
        // with a horizontal rule followed by a code block.
        s = format!("Token: `{new_tok}`\n");
      }
      None => {
        s.push('\n');
        s.push_str(line);
      }
    }
  }
  assert!(doc_map.insert(tok.unwrap(), s).is_none());
  let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR should be set");
  gen(
    std::path::Path::new(out_dir.as_str()),
    "SML",
    &["Whitespace", "BlockComment", "Invalid"],
    include_str!("syntax.ungram").parse().expect("ungram parse"),
    |s| {
      let kind: TokenKind;
      let mut name: String;
      let mut desc = None::<String>;
      if let Some(d) = SPECIAL.iter().find_map(|&(n, d)| (s == n).then_some(d)) {
        kind = TokenKind::Special;
        name = s.to_owned();
        desc = Some(d.to_owned());
      } else if s.chars().any(|c| c.is_ascii_alphabetic()) {
        kind = TokenKind::Keyword;
        name = snake_to_pascal(s);
        name.push_str("Kw");
      } else {
        kind = TokenKind::Punctuation;
        name = String::new();
        for c in s.chars() {
          name.push_str(char_name::get(c));
        }
      }
      let doc = doc_map.get(s).cloned();
      (kind, Token { name, desc, doc })
    },
  )
}
