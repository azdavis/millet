//! Handle doc comments.

use sml_syntax::{SyntaxKind as SK, SyntaxNode};

/// Adds doc comments in the `root` to the `info`.
pub fn get(root: &SyntaxNode, low: &sml_lower::Lower, info: &mut sml_statics::Info) {
  let indices = std::iter::empty()
    .chain(low.arenas.pat.iter().map(|(x, _)| sml_hir::Idx::Pat(x)))
    .chain(low.arenas.dec.iter().map(|(x, _)| sml_hir::Idx::Dec(x)))
    .chain(low.arenas.spec.iter().map(|(x, _)| sml_hir::Idx::Spec(x)));
  for idx in indices {
    let ptr = low.ptrs.hir_to_ast(idx).expect("no syntax ptr");
    let node = ptr.to_node(root);
    if let Some(doc) = get_comment(&node) {
      info.add_doc(idx, doc);
    }
  }
}

fn get_comment(node: &SyntaxNode) -> Option<String> {
  let mut tok = node.first_token()?;
  let mut saw_one = false;
  loop {
    match tok.kind() {
      SK::BlockComment => break,
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
