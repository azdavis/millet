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

fn code_h2(s: &str) -> Option<&str> {
  s.strip_prefix("## `")?.strip_suffix('`')
}

fn main() -> std::io::Result<()> {
  let mut doc_map = FxHashMap::<&str, String>::default();
  let mut key = None::<&str>;
  let mut val = String::new();
  for line in include_str!("../../docs/tokens.md").lines() {
    match code_h2(line) {
      Some(next) => {
        if let Some(key) = key {
          assert!(doc_map.insert(key, val).is_none());
        }
        key = Some(next);
        // NOTE: this used to be a sml code block, but the VS Code markdown viewer doesn't do well
        // with a horizontal rule followed by a code block.
        val = format!("Token: `{next}`\n");
      }
      None => {
        val.push('\n');
        val.push_str(line);
      }
    }
  }
  assert!(doc_map.insert(key.unwrap(), val).is_none());
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
