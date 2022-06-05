use identifier_case::snake_to_pascal;
use syntax_gen::{gen, TokenKind};

const SPECIAL: [(&str, &str); 7] = [
  ("Name", "a name"),
  ("TyVar", "a type variable"),
  ("IntLit", "an integer literal"),
  ("RealLit", "a real literal"),
  ("WordLit", "a word literal"),
  ("CharLit", "a character literal"),
  ("StringLit", "a string literal"),
];

fn get_token(name: &str) -> (TokenKind, String) {
  if let Some(desc) = SPECIAL.iter().find_map(|&(n, d)| (name == n).then(|| d)) {
    (TokenKind::Special(desc), name.to_owned())
  } else if name.chars().any(|x| x.is_ascii_alphabetic()) {
    let mut ret = snake_to_pascal(name);
    ret.push_str("Kw");
    (TokenKind::Keyword, ret)
  } else {
    let mut ret = String::new();
    for c in name.chars() {
      ret.push_str(char_name::get(c));
    }
    (TokenKind::Punctuation, ret)
  }
}

fn main() {
  gen(
    "SML",
    &["Whitespace", "BlockComment", "Invalid"],
    include_str!("syntax.ungram").parse().unwrap(),
    get_token,
  )
  .unwrap();
}
