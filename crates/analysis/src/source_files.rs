//! Helpers for working with source files.

use paths::{PathMap, WithPath};
use sml_syntax::ast::{AstNode as _, SyntaxNodePtr};
use sml_syntax::{rowan::TokenAtOffset, SyntaxKind, SyntaxToken};
use text_pos::{PositionUtf16, RangeUtf16};

pub(crate) fn path_and_range(
  source_files: &PathMap<mlb_statics::SourceFile>,
  idx: WithPath<sml_hir::Idx>,
) -> Option<WithPath<RangeUtf16>> {
  let def_file = source_files.get(&idx.path)?;
  let ptr = def_file.syntax.lower.ptrs.hir_to_ast(idx.val)?;
  Some(idx.path.wrap(def_file.syntax.pos_db.range_utf16(ptr.text_range())?))
}

pub(crate) fn file_and_token(
  source_files: &PathMap<mlb_statics::SourceFile>,
  pos: WithPath<PositionUtf16>,
) -> Option<FileAndToken<'_>> {
  let file = source_files.get(&pos.path)?;
  let offset = file.syntax.pos_db.text_size_utf16(pos.val)?;
  if !file.syntax.parse.root.syntax().text_range().contains(offset) {
    return None;
  }
  let token = match file.syntax.parse.root.syntax().token_at_offset(offset) {
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

  pub(crate) fn get_ptr_and_indices(&self) -> Option<(SyntaxNodePtr, &[sml_hir::Idx])> {
    let mut node = self.token.parent()?;
    loop {
      let ptr = SyntaxNodePtr::new(&node);
      match self.file.syntax.lower.ptrs.ast_to_hir_all(&ptr) {
        Some(indices) => return Some((ptr, indices)),
        None => node = node.parent()?,
      }
    }
  }
}

fn priority(kind: SyntaxKind) -> u8 {
  match kind {
    SyntaxKind::Name => 5,
    SyntaxKind::OpKw | SyntaxKind::Dot => 4,
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
