use fast_hash::FxHashMap;
use std::fmt;
use syntax::ast::{self, AstNode, AstPtr};
use syntax::rowan::TextRange;

pub(crate) type SyntaxNodePtr = ast::SyntaxNodePtr<syntax::SML>;

/// Pointers between the AST and the HIR.
#[derive(Debug, Default)]
#[allow(missing_docs)]
pub struct Ptrs {
  top_dec: BiMap<ast::TopDec, hir::TopDec>,
  str_dec_one: BiMap<ast::StrDecOne, hir::StrDec>,
  str_dec: BiMap<ast::StrDec, hir::StrDec>,
  str_exp: BiMap<ast::StrExp, hir::StrExp>,
  sig_exp: BiMap<ast::SigExp, hir::SigExp>,
  spec_one: BiMap<ast::SpecOne, hir::Spec>,
  spec: BiMap<ast::Spec, hir::Spec>,
  exp: BiMap<ast::Exp, hir::Exp>,
  dec_one: BiMap<ast::DecOne, hir::Dec>,
  dec_in_exp: BiMap<ast::Exp, hir::Dec>,
  dec: BiMap<ast::Dec, hir::Dec>,
  pat: BiMap<ast::Pat, hir::Pat>,
  pat_in_exp: BiMap<ast::Exp, hir::Pat>,
  ty: BiMap<ast::Ty, hir::Ty>,
}

macro_rules! try_get_hir {
  ($idx:ident, $map:expr) => {
    if let Some(x) = $map.hir_to_ast.get($idx) {
      return Some(x.syntax_node_ptr());
    }
  };
}

impl Ptrs {
  /// Returns the `SyntaxNodePtr` for an HIR index.
  pub fn get(&self, idx: hir::Idx) -> Option<SyntaxNodePtr> {
    match idx {
      hir::Idx::Exp(idx) => try_get_hir!(idx, self.exp),
      hir::Idx::Pat(idx) => {
        try_get_hir!(idx, self.pat);
        try_get_hir!(idx, self.pat_in_exp);
      }
      hir::Idx::Ty(idx) => try_get_hir!(idx, self.ty),
      hir::Idx::Dec(idx) => {
        try_get_hir!(idx, self.dec);
        try_get_hir!(idx, self.dec_one);
        try_get_hir!(idx, self.dec_in_exp);
      }
      hir::Idx::StrExp(idx) => try_get_hir!(idx, self.str_exp),
      hir::Idx::StrDec(idx) => {
        try_get_hir!(idx, self.str_dec);
        try_get_hir!(idx, self.str_dec_one);
      }
      hir::Idx::SigExp(idx) => try_get_hir!(idx, self.sig_exp),
      hir::Idx::Spec(idx) => {
        try_get_hir!(idx, self.spec);
        try_get_hir!(idx, self.spec_one);
      }
      hir::Idx::TopDec(idx) => try_get_hir!(idx, self.top_dec),
    }
    None
  }
}

pub(crate) struct BiMap<A, H>
where
  A: AstNode,
{
  hir_to_ast: hir::la_arena::ArenaMap<hir::la_arena::Idx<H>, AstPtr<A>>,
  ast_to_hir: FxHashMap<AstPtr<A>, hir::la_arena::Idx<H>>,
}

impl<A, H> BiMap<A, H>
where
  A: AstNode,
{
  fn insert(&mut self, idx: hir::la_arena::Idx<H>, ptr: AstPtr<A>) {
    self.hir_to_ast.insert(idx, ptr.clone());
    self.ast_to_hir.insert(ptr, idx);
  }
}

impl<N, I> fmt::Debug for BiMap<N, I>
where
  N: AstNode,
  I: fmt::Debug,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_struct("BiMap")
      .field("hir_to_ast", &self.hir_to_ast)
      .field("ast_to_hir", &self.ast_to_hir)
      .finish()
  }
}

impl<N, I> Default for BiMap<N, I>
where
  N: AstNode,
{
  fn default() -> Self {
    Self {
      hir_to_ast: Default::default(),
      ast_to_hir: Default::default(),
    }
  }
}

#[derive(Debug)]
pub struct Error {
  pub range: TextRange,
  pub kind: ErrorKind,
}

#[derive(Debug)]
pub enum ErrorKind {
  Unsupported,
  FunBindMismatchedName(String, String),
  FunBindWrongNumPats(usize, usize),
  InvalidIntLit(std::num::ParseIntError),
  InvalidRealLit(std::num::ParseFloatError),
}

impl fmt::Display for ErrorKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ErrorKind::Unsupported => f.write_str("unsupported language construct"),
      ErrorKind::FunBindMismatchedName(want, got) => {
        write!(f, "mismatched names: expected {want}, found {got}")
      }
      ErrorKind::FunBindWrongNumPats(want, got) => {
        write!(f, "wrong number of patterns: expected {want}, found {got}")
      }
      ErrorKind::InvalidIntLit(e) => write!(f, "invalid literal: {e}"),
      ErrorKind::InvalidRealLit(e) => write!(f, "invalid literal: {e}"),
    }
  }
}

/// The result of lowering.
#[derive(Debug)]
pub struct Lower {
  /// The errors.
  pub errors: Vec<Error>,
  /// The arenas.
  pub arenas: hir::Arenas,
  /// The pointers.
  pub ptrs: Ptrs,
  /// The top-level declarations, in order.
  pub top_decs: Vec<hir::TopDecIdx>,
}

#[derive(Debug, Default)]
pub(crate) struct Cx {
  fresh_idx: u32,
  errors: Vec<Error>,
  arenas: hir::Arenas,
  ptrs: Ptrs,
}

impl Cx {
  /// Returns a `Name` that is both:
  /// - not writeable in user code, and will thus not collide with any identifiers in user code;
  /// - distinct from all other `Name`s returned from self thus far, and will thus not collide
  ///   with any of those.
  pub(crate) fn fresh(&mut self) -> hir::Name {
    let ret = hir::Name::new(self.fresh_idx.to_string());
    self.fresh_idx += 1;
    ret
  }

  pub(crate) fn err(&mut self, range: TextRange, kind: ErrorKind) {
    self.errors.push(Error { range, kind })
  }

  pub(crate) fn finish(self, top_decs: Vec<hir::TopDecIdx>) -> Lower {
    Lower {
      errors: self.errors,
      arenas: self.arenas,
      ptrs: self.ptrs,
      top_decs,
    }
  }

  pub(crate) fn top_dec(&mut self, val: hir::TopDec, ptr: AstPtr<ast::TopDec>) -> hir::TopDecIdx {
    let idx = self.arenas.top_dec.alloc(val);
    self.ptrs.top_dec.insert(idx, ptr);
    idx
  }

  pub(crate) fn str_dec_one(
    &mut self,
    val: hir::StrDec,
    ptr: AstPtr<ast::StrDecOne>,
  ) -> hir::StrDecIdx {
    let idx = self.arenas.str_dec.alloc(val);
    self.ptrs.str_dec_one.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn str_dec_seq(
    &mut self,
    val: Vec<hir::StrDecIdx>,
    ptr: AstPtr<ast::StrDec>,
  ) -> hir::StrDecIdx {
    let idx = self.arenas.str_dec.alloc(hir::StrDec::Seq(val));
    self.ptrs.str_dec.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn str_exp(&mut self, val: hir::StrExp, ptr: AstPtr<ast::StrExp>) -> hir::StrExpIdx {
    let idx = self.arenas.str_exp.alloc(val);
    self.ptrs.str_exp.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn sig_exp(&mut self, val: hir::SigExp, ptr: AstPtr<ast::SigExp>) -> hir::SigExpIdx {
    let idx = self.arenas.sig_exp.alloc(val);
    self.ptrs.sig_exp.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn spec_one(&mut self, val: hir::Spec, ptr: AstPtr<ast::SpecOne>) -> hir::SpecIdx {
    let idx = self.arenas.spec.alloc(val);
    self.ptrs.spec_one.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn spec_seq(
    &mut self,
    val: Vec<hir::SpecIdx>,
    ptr: AstPtr<ast::Spec>,
  ) -> hir::SpecIdx {
    let idx = self.arenas.spec.alloc(hir::Spec::Seq(val));
    self.ptrs.spec.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn exp(&mut self, val: hir::Exp, ptr: AstPtr<ast::Exp>) -> hir::ExpIdx {
    let idx = self.arenas.exp.alloc(val);
    self.ptrs.exp.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn dec_one(&mut self, val: hir::Dec, ptr: AstPtr<ast::DecOne>) -> hir::DecIdx {
    let idx = self.arenas.dec.alloc(val);
    self.ptrs.dec_one.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn dec_in_exp(&mut self, val: hir::Dec, ptr: AstPtr<ast::Exp>) -> hir::DecIdx {
    let idx = self.arenas.dec.alloc(val);
    self.ptrs.dec_in_exp.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn dec_seq(&mut self, val: Vec<hir::DecIdx>, ptr: AstPtr<ast::Dec>) -> hir::DecIdx {
    let idx = self.arenas.dec.alloc(hir::Dec::Seq(val));
    self.ptrs.dec.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn pat(&mut self, val: hir::Pat, ptr: AstPtr<ast::Pat>) -> hir::PatIdx {
    let idx = self.arenas.pat.alloc(val);
    self.ptrs.pat.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn pat_in_exp(&mut self, val: hir::Pat, ptr: AstPtr<ast::Exp>) -> hir::PatIdx {
    let idx = self.arenas.pat.alloc(val);
    self.ptrs.pat_in_exp.insert(idx, ptr);
    Some(idx)
  }

  pub(crate) fn ty(&mut self, val: hir::Ty, ptr: AstPtr<ast::Ty>) -> hir::TyIdx {
    let idx = self.arenas.ty.alloc(val);
    self.ptrs.ty.insert(idx, ptr);
    Some(idx)
  }
}
