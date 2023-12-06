//! Types for working with SML syntax trees.

#[allow(clippy::pedantic, missing_debug_implementations, missing_docs)]
pub mod ast {
  include!(concat!(env!("OUT_DIR"), "/ast.rs"));
}

#[allow(clippy::pedantic, missing_debug_implementations, missing_docs)]
pub mod kind {
  include!(concat!(env!("OUT_DIR"), "/kind.rs"));
}

use ast::AstNode as _;

fn custom_node_range(node: kind::SyntaxNode) -> Option<rowan::TextRange> {
  if let Some(node) = ast::CaseExp::cast(node.clone()) {
    let case_kw = node.case_kw()?;
    let of_kw = node.of_kw()?;
    return Some(rowan::TextRange::new(case_kw.text_range().start(), of_kw.text_range().end()));
  }
  if let Some(node) = ast::LetExp::cast(node.clone()) {
    return Some(node.let_kw()?.text_range());
  }
  if let Some(node) = ast::LocalDec::cast(node.clone()) {
    return Some(node.local_dec_hd()?.local_kw()?.text_range());
  }
  if let Some(node) = ast::LetStrExp::cast(node.clone()) {
    return Some(node.let_kw()?.text_range());
  }
  if let Some(node) = ast::StructStrExp::cast(node.clone()) {
    return Some(node.struct_kw()?.text_range());
  }
  if let Some(node) = ast::SigSigExp::cast(node) {
    return Some(node.sig_kw()?.text_range());
  }
  None
}

/// Returns the node range for the node, which is either a custom node range to allow for better
/// readability or the whole actual range of the node.
///
/// For example, given a node for a `case` expressions, this will return node range that only covers
/// the `case ... of`. This is so the range is across fewer (usually one) line(s) than if we used
/// the range of the whole `case` and all of its matcher arms.
#[must_use]
pub fn node_range(node: &kind::SyntaxNode) -> rowan::TextRange {
  custom_node_range(node.clone()).unwrap_or_else(|| node.text_range())
}

/// Returns the best token in the node at the offset.
#[must_use]
pub fn node_token(syntax: &kind::SyntaxNode, offset: rowan::TextSize) -> Option<kind::SyntaxToken> {
  match syntax.token_at_offset(offset) {
    rowan::TokenAtOffset::None => None,
    rowan::TokenAtOffset::Single(t) => Some(t),
    rowan::TokenAtOffset::Between(t1, t2) => {
      Some(if priority(t1.kind()) >= priority(t2.kind()) { t1 } else { t2 })
    }
  }
}

fn priority(kind: kind::SyntaxKind) -> u8 {
  match kind {
    kind::SyntaxKind::Name => 5,
    kind::SyntaxKind::OpKw | kind::SyntaxKind::Dot => 4,
    kind::SyntaxKind::LRound
    | kind::SyntaxKind::RRound
    | kind::SyntaxKind::LCurly
    | kind::SyntaxKind::RCurly => 3,
    kind::SyntaxKind::Comma
    | kind::SyntaxKind::Colon
    | kind::SyntaxKind::Star
    | kind::SyntaxKind::Eq => 2,
    kind::SyntaxKind::Whitespace | kind::SyntaxKind::BlockComment | kind::SyntaxKind::Invalid => 0,
    _ => 1,
  }
}
