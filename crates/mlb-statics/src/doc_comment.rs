//! Handle doc comments.

use syntax::{SyntaxKind, SyntaxNode};

/// Adds doc comments in the `root` to the `info`.
pub fn get(root: &SyntaxNode, low: &lower::Lower, info: &mut statics::Info) {
  let indices = std::iter::empty()
    .chain(low.arenas.spec.iter().map(|(x, _)| hir::Idx::Spec(x)))
    .chain(low.arenas.pat.iter().map(|(x, _)| hir::Idx::Pat(x)));
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
  while tok.kind() != SyntaxKind::BlockComment {
    // does this have to be so complicated? i'm just trying to, given a token, walk backwards up the
    // tree and visit every token.
    tok = match tok.prev_token() {
      Some(t) => t,
      None => {
        let mut node = tok.parent()?;
        loop {
          match node.prev_sibling_or_token() {
            Some(x) => match x {
              syntax::rowan::NodeOrToken::Node(n) => match n.last_token() {
                Some(t) => break t,
                None => node = n,
              },
              syntax::rowan::NodeOrToken::Token(t) => break t,
            },
            None => node = node.parent()?,
          }
        }
      }
    };
  }
  let mut lines: Vec<_> = tok.text().lines().map(str::trim).collect();
  let is_doc_comment = !lines.is_empty() && lines.remove(0) == "(*!" && lines.pop()? == "!*)";
  is_doc_comment.then(|| lines.join("\n"))
}
