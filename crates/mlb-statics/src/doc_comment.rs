//! Handle doc comments.

use sml_syntax::{SyntaxKind, SyntaxNode};

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
  while tok.kind() != SyntaxKind::BlockComment {
    // does this have to be so complicated? i'm just trying to, given a token, walk backwards up the
    // tree and visit every token.
    //
    // TODO stop walking up if we hit another dec or something? currently given this:
    //
    // ```sml
    // (*!
    //  * Foo the bar.
    //  *)
    // fun foo () = ()
    //
    // fun quz () = ()
    // ```
    //
    // not only `foo` but also `quz` will have the "Foo the bar." doc.
    tok = match tok.prev_token() {
      Some(t) => t,
      None => {
        let mut node = tok.parent()?;
        loop {
          match node.prev_sibling_or_token() {
            Some(x) => match x {
              sml_syntax::rowan::NodeOrToken::Node(n) => match n.last_token() {
                Some(t) => break t,
                None => node = n,
              },
              sml_syntax::rowan::NodeOrToken::Token(t) => break t,
            },
            None => node = node.parent()?,
          }
        }
      }
    };
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
