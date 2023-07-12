//! Types for working with SML syntax trees.

#![deny(rust_2018_idioms)]

pub mod ast {
  include!(concat!(env!("OUT_DIR"), "/ast.rs"));
}
mod kind {
  include!(concat!(env!("OUT_DIR"), "/kind.rs"));
}

pub use kind::*;
pub use rowan;
pub use token;

use ast::AstNode as _;

fn custom_node_range(node: SyntaxNode) -> Option<rowan::TextRange> {
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

pub fn node_range(node: &SyntaxNode) -> rowan::TextRange {
  custom_node_range(node.clone()).unwrap_or_else(|| node.text_range())
}
