//! Helpers for working with source files.

use paths::{PathMap, WithPath};
use sml_syntax::ast::{AstNode as _, SyntaxNodePtr};
use sml_syntax::{rowan::TokenAtOffset, SyntaxKind, SyntaxToken};
use text_pos::{Position, Range};

pub(crate) fn path_and_range(
  source_files: &PathMap<mlb_statics::SourceFile>,
  def: sml_statics::def::Def,
) -> Option<WithPath<Range>> {
  let (path, idx) = match def {
    sml_statics::def::Def::Path(sml_statics::def::Path::Regular(a), b) => (a, b),
    sml_statics::def::Def::Path(sml_statics::def::Path::BuiltinLib(_), _)
    | sml_statics::def::Def::Primitive(_) => return None,
  };
  let def_file = source_files.get(&path)?;
  let ptr = def_file.syntax.lower.ptrs.hir_to_ast(idx)?;
  let def_range = ptr.to_node(def_file.syntax.parse.root.syntax()).text_range();
  Some(path.wrap(def_file.syntax.pos_db.range(def_range)?))
}
pub(crate) fn file_and_token(
  source_files: &PathMap<mlb_statics::SourceFile>,
  pos: WithPath<Position>,
) -> Option<FileAndToken<'_>> {
  let file = source_files.get(&pos.path)?;
  let idx = file.syntax.pos_db.text_size(pos.val)?;
  if !file.syntax.parse.root.syntax().text_range().contains(idx) {
    return None;
  }
  let token = match file.syntax.parse.root.syntax().token_at_offset(idx) {
    TokenAtOffset::None => return None,
    TokenAtOffset::Single(t) => t,
    TokenAtOffset::Between(t1, t2) => {
      if priority(t1.kind()) >= priority(t2.kind()) {
        t1
      } else {
        t2
      }
    }
  };
  Some(FileAndToken { file, token })
}

pub(crate) struct FileAndToken<'a> {
  pub(crate) file: &'a mlb_statics::SourceFile,
  pub(crate) token: SyntaxToken,
}

impl FileAndToken<'_> {
  pub(crate) fn get_ptr_and_idx(&self) -> Option<(SyntaxNodePtr, sml_hir::Idx)> {
    let mut node = self.token.parent()?;
    loop {
      let ptr = SyntaxNodePtr::new(&node);
      match self.file.syntax.lower.ptrs.ast_to_hir(&ptr) {
        Some(idx) => return Some((ptr, idx)),
        None => node = node.parent()?,
      }
    }
  }
}

fn priority(kind: SyntaxKind) -> u8 {
  match kind {
    SyntaxKind::Name => 5,
    SyntaxKind::OpKw => 4,
    SyntaxKind::TyVar => 3,
    SyntaxKind::CharLit
    | SyntaxKind::IntLit
    | SyntaxKind::RealLit
    | SyntaxKind::StringLit
    | SyntaxKind::WordLit => 2,
    SyntaxKind::Whitespace | SyntaxKind::BlockComment | SyntaxKind::Invalid => 0,
    _ => 1,
  }
}
