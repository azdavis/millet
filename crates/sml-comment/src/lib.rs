//! Extracting interesting comments from SML files.

use sml_syntax::kind::{SyntaxKind as SK, SyntaxNode, SyntaxToken};

/// Gets the comment above this node, then parses the doc string out of it.
#[must_use]
pub fn doc_comment_above(node: &SyntaxNode) -> Option<String> {
  let tok = comment_above(node)?;
  let mut lines: Vec<_> = tok
    .text()
    .lines()
    .filter_map(|line| {
      let (_, s) = line.split_once('*')?;
      Some(s.strip_prefix(' ').unwrap_or(s))
    })
    .collect();
  let is_doc_comment = !lines.is_empty() && lines.remove(0) == "!" && lines.pop()? == ")";
  is_doc_comment.then(|| lines.join("\n"))
}

/// Returns the comment "above" this node. The node must be one that comments may be "above", like
/// `val` or `fun` or `and`.
#[must_use]
pub fn comment_above(node: &SyntaxNode) -> Option<SyntaxToken> {
  let mut tok = node.first_token()?;
  let mut saw_one = false;
  loop {
    match tok.kind() {
      SK::BlockComment => return Some(tok),
      SK::DotDotDot
      | SK::AndKw
      | SK::ValKw
      | SK::FunKw
      | SK::TypeKw
      | SK::EqtypeKw
      | SK::DatatypeKw
      | SK::AbstypeKw
      | SK::ExceptionKw
      | SK::OpenKw
      | SK::InfixKw
      | SK::InfixrKw
      | SK::NonfixKw
      | SK::DoKw
      | SK::LocalKw
      | SK::StructureKw
      | SK::SignatureKw
      | SK::FunctorKw
      | SK::IncludeKw => {
        if saw_one {
          return None;
        }
        saw_one = true;
      }
      _ => {}
    }
    tok = tok.prev_token()?;
  }
}
