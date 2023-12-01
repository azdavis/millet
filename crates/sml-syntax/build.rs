//! Generating Rust code from the ungrammar.

use fast_hash::{map, FxHashMap};

fn main() {
  let doc = code_h2_md_map::get(include_str!("../../docs/tokens.md"), |tok| {
    // NOTE: this used to be a sml code block, but the VS Code Markdown viewer doesn't do well with
    // a horizontal rule followed by a code block.
    format!("Token: `{tok}`\n")
  });
  let doc: FxHashMap<_, _> = doc.iter().map(|(&k, v)| (k, v.as_str())).collect();
  let special = map([
    ("Name", "a name"),
    ("TyVar", "a type variable"),
    ("IntLit", "an integer literal"),
    ("RealLit", "a real literal"),
    ("WordLit", "a word literal"),
    ("CharLit", "a character literal"),
    ("StringLit", "a string literal"),
  ]);
  let options = syntax_gen::Options {
    lang: "SML",
    trivia: &["Whitespace", "BlockComment", "Invalid"],
    grammar: include_str!("syntax.ungram"),
    doc: &doc,
    special: &special,
    file: file!(),
  };
  syntax_gen::gen(&options);
}
