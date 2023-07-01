//! Utilities.

use config::lang::Language;
use diagnostic::{Code, Severity};
use fast_hash::FxHashMap;
use sml_syntax::{ast::SyntaxNodePtr, rowan::TextRange};
use std::fmt;

/// Pointers between the AST and the HIR.
#[derive(Debug, Default)]
pub struct Ptrs {
  hir_to_ast: FxHashMap<sml_hir::Idx, SyntaxNodePtr>,
  /// allow a multi mapping for this direction
  ast_to_hir: FxHashMap<SyntaxNodePtr, Vec<sml_hir::Idx>>,
}

impl Ptrs {
  /// Returns the unique syntax pointer for an HIR index.
  #[must_use]
  pub fn hir_to_ast(&self, idx: sml_hir::Idx) -> Option<SyntaxNodePtr> {
    self.hir_to_ast.get(&idx).cloned()
  }

  /// Returns one of possibly many HIR indices for the syntax pointer.
  #[must_use]
  pub fn ast_to_hir(&self, ptr: &SyntaxNodePtr) -> Option<sml_hir::Idx> {
    // prefer newer
    self.ast_to_hir.get(ptr)?.last().copied()
  }

  /// Returns all of the possibly many HIR indices for the syntax pointer.
  #[must_use]
  pub fn ast_to_hir_all(&self, ptr: &SyntaxNodePtr) -> Option<&[sml_hir::Idx]> {
    self.ast_to_hir.get(ptr).map(Vec::as_slice)
  }

  fn insert(&mut self, idx: sml_hir::Idx, ptr: SyntaxNodePtr) {
    assert!(self.hir_to_ast.insert(idx, ptr.clone()).is_none());
    self.ast_to_hir.entry(ptr).or_default().push(idx);
  }
}

#[derive(Debug)]
pub(crate) enum ErrorKind {
  /// must be first here, but have the highest error codes
  Unsupported(&'static str),
  FunBindMismatchedName(String, String),
  FunBindWrongNumPats(usize, usize),
  InvalidIntLit(sml_hir::ParseIntError),
  InvalidWordLit(std::num::ParseIntError),
  InvalidRealLit(std::num::ParseFloatError),
  InvalidNumLab(std::num::ParseIntError),
  ZeroNumLab,
  MultipleRestPatRows,
  RestPatRowNotLast,
  PrecedingBar,
  RequiresOperand,
  DecNotAllowedHere,
  OpBoolBinOp,
  ExpNotAllowedHere,
  NonSpecDecSyntax,
  UnnecessaryParens,
  ComplexBoolExp,
  OneArmedCase,
  UnnecessarySemicolon,
  MultipleTypedPat,
  MissingRhs,
  InvalidSharingType,
  InvalidEqtype,
  DecHole,
  NotSpec,
  AsPatLhsNotName,
  PatNameIsNameOfContainingFun(MatcherFlavor),
  EmptyFun,
  EmptyExpSemiSeq,
  Trailing(Sep),
  Disallowed(Item),
  TopLevelOpen,
  InvalidOpaqueAscription,
  ExceptionCopyRhsNotPath,
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      ErrorKind::Unsupported(s) => write!(f, "unsupported: {s}"),
      ErrorKind::FunBindMismatchedName(want, got) => {
        write!(f, "expected a function clause for `{want}`, found one for `{got}`")
      }
      ErrorKind::FunBindWrongNumPats(want, got) => {
        let s = if *want == 1 { "" } else { "s" };
        write!(f, "expected {want} pattern{s}, found {got}")
      }
      ErrorKind::InvalidIntLit(e) => write!(f, "invalid literal: {e}"),
      ErrorKind::InvalidWordLit(e) => write!(f, "invalid literal: {e}"),
      ErrorKind::InvalidRealLit(e) => write!(f, "invalid literal: {e}"),
      ErrorKind::InvalidNumLab(e) => write!(f, "invalid numeric label: {e}"),
      ErrorKind::ZeroNumLab => f.write_str("invalid numeric label: numeric labels start at 1"),
      ErrorKind::MultipleRestPatRows => f.write_str("multiple `...`"),
      ErrorKind::RestPatRowNotLast => f.write_str("`...` must come last"),
      ErrorKind::PrecedingBar => f.write_str("preceding `|`"),
      ErrorKind::RequiresOperand => f.write_str("requires at least 1 operand"),
      ErrorKind::DecNotAllowedHere => f.write_str("structure-level declaration not allowed here"),
      ErrorKind::OpBoolBinOp => f.write_str("`andalso` and `orelse` not allowed with `op`"),
      ErrorKind::ExpNotAllowedHere => f.write_str("expression not allowed here"),
      ErrorKind::NonSpecDecSyntax => {
        f.write_str("specification uses declaration syntax not allowed here")
      }
      ErrorKind::UnnecessaryParens => f.write_str("unnecessary parentheses"),
      ErrorKind::ComplexBoolExp => f.write_str("overly complex `bool` expression"),
      ErrorKind::OneArmedCase => f.write_str("`case` with only one arm"),
      ErrorKind::UnnecessarySemicolon => f.write_str("unnecessary `;`"),
      ErrorKind::MultipleTypedPat => f.write_str("multiple types on one pattern"),
      ErrorKind::MissingRhs => f.write_str("missing right-hand side of declaration"),
      ErrorKind::InvalidSharingType => f.write_str("`sharing type` not allowed here"),
      ErrorKind::InvalidEqtype => f.write_str("`eqtype` not allowed here"),
      ErrorKind::DecHole => f.write_str("declaration hole"),
      ErrorKind::NotSpec => f.write_str("non-specification not allowed here"),
      ErrorKind::AsPatLhsNotName => f.write_str("left-hand side of `as` pattern must be a name"),
      ErrorKind::PatNameIsNameOfContainingFun(flavor) => {
        write!(f, "name bound in pattern inside a `{flavor}` matches name of a `fun` that contains the `{flavor}`")
      }
      ErrorKind::EmptyFun => f.write_str("`fun` requires at least 1 parameter"),
      ErrorKind::EmptyExpSemiSeq => {
        f.write_str("`;`-separated sequence requires at least 1 expression")
      }
      ErrorKind::Trailing(s) => write!(f, "trailing `{s}`"),
      ErrorKind::Disallowed(item) => write!(f, "disallowed {item}"),
      ErrorKind::TopLevelOpen => write!(f, "top-level `open`"),
      ErrorKind::InvalidOpaqueAscription => f.write_str("opaque ascription not allowed here"),
      ErrorKind::ExceptionCopyRhsNotPath => {
        f.write_str("right-hand side of `exception` copy declaration must be a path")
      }
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum MatcherFlavor {
  Case,
  Fn,
  Handle,
}

impl fmt::Display for MatcherFlavor {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      MatcherFlavor::Case => f.write_str("case"),
      MatcherFlavor::Fn => f.write_str("fn"),
      MatcherFlavor::Handle => f.write_str("handle"),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Sep {
  Comma,
  Semi,
}

impl fmt::Display for Sep {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Sep::Comma => f.write_str(","),
      Sep::Semi => f.write_str(";"),
    }
  }
}

#[derive(Debug)]
pub(crate) enum Item {
  Exp(&'static str),
  Dec(&'static str),
}

impl fmt::Display for Item {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Item::Exp(s) => write!(f, "expression: {s}"),
      Item::Dec(s) => write!(f, "declaration: {s}"),
    }
  }
}

/// An error when lowering.
#[derive(Debug)]
pub struct Error {
  range: TextRange,
  kind: ErrorKind,
}

impl Error {
  /// Returns the range for this.
  #[must_use]
  pub fn range(&self) -> TextRange {
    self.range
  }

  /// Returns the code for this.
  #[must_use]
  pub fn code(&self) -> Code {
    match self.kind {
      ErrorKind::Unsupported(_) => Code::n(4999),
      ErrorKind::FunBindMismatchedName(_, _) => Code::n(4001),
      ErrorKind::FunBindWrongNumPats(_, _) => Code::n(4002),
      ErrorKind::InvalidIntLit(_) | ErrorKind::InvalidWordLit(_) => Code::n(4003),
      ErrorKind::InvalidRealLit(_) => Code::n(4004),
      ErrorKind::InvalidNumLab(_) | ErrorKind::ZeroNumLab => Code::n(4005),
      ErrorKind::MultipleRestPatRows => Code::n(4006),
      ErrorKind::RestPatRowNotLast => Code::n(4007),
      ErrorKind::PrecedingBar => Code::n(4008),
      ErrorKind::RequiresOperand => Code::n(4009),
      ErrorKind::DecNotAllowedHere => Code::n(4010),
      ErrorKind::OpBoolBinOp => Code::n(4011),
      ErrorKind::ExpNotAllowedHere => Code::n(4012),
      ErrorKind::NonSpecDecSyntax => Code::n(4013),
      ErrorKind::UnnecessaryParens => Code::n(4014),
      ErrorKind::ComplexBoolExp => Code::n(4015),
      ErrorKind::OneArmedCase => Code::n(4016),
      ErrorKind::UnnecessarySemicolon => Code::n(4017),
      ErrorKind::MultipleTypedPat => Code::n(4018),
      ErrorKind::MissingRhs => Code::n(4019),
      ErrorKind::InvalidSharingType => Code::n(4020),
      ErrorKind::InvalidEqtype => Code::n(4021),
      ErrorKind::DecHole => Code::n(4022),
      ErrorKind::NotSpec => Code::n(4023),
      ErrorKind::AsPatLhsNotName => Code::n(4024),
      ErrorKind::PatNameIsNameOfContainingFun(_) => Code::n(4025),
      ErrorKind::EmptyFun => Code::n(4026),
      ErrorKind::EmptyExpSemiSeq => Code::n(4027),
      ErrorKind::Trailing(_) => Code::n(4028),
      ErrorKind::Disallowed(_) => Code::n(4029),
      ErrorKind::TopLevelOpen => Code::n(4030),
      ErrorKind::InvalidOpaqueAscription => Code::n(4031),
      ErrorKind::ExceptionCopyRhsNotPath => Code::n(4032),
    }
  }

  /// Returns the severity for this.
  #[must_use]
  pub fn severity(&self) -> Severity {
    match self.kind {
      ErrorKind::UnnecessaryParens
      | ErrorKind::ComplexBoolExp
      | ErrorKind::OneArmedCase
      | ErrorKind::UnnecessarySemicolon
      | ErrorKind::MultipleTypedPat
      | ErrorKind::PatNameIsNameOfContainingFun(_)
      | ErrorKind::TopLevelOpen => Severity::Warning,
      _ => Severity::Error,
    }
  }
}

/// The result of lowering.
#[derive(Debug)]
pub struct Lower {
  /// The errors.
  pub errors: Vec<Error>,
  /// The arenas.
  pub arenas: sml_hir::Arenas,
  /// The pointers.
  pub ptrs: Ptrs,
  /// The top declarations.
  pub root: sml_hir::StrDecSeq,
}

#[derive(Debug)]
pub(crate) struct St<'a> {
  fresh_idx: u32,
  errors: Vec<Error>,
  arenas: sml_hir::Arenas,
  ptrs: Ptrs,
  fun_names: Vec<str_util::Name>,
  lang: &'a Language,
  level: usize,
}

#[allow(clippy::unnecessary_wraps)]
impl<'a> St<'a> {
  pub(crate) fn new(lang: &'a Language) -> St<'a> {
    St {
      fresh_idx: 0,
      errors: Vec::new(),
      arenas: sml_hir::Arenas::default(),
      ptrs: Ptrs::default(),
      fun_names: Vec::new(),
      lang,
      level: 0,
    }
  }

  /// Returns a `Name` that is both:
  /// - not writeable in user code, and will thus not collide with any identifiers in user code;
  /// - distinct from all other `Name`s returned from self thus far, and will thus not collide
  ///   with any of those.
  pub(crate) fn fresh(&mut self) -> str_util::Name {
    let ret = str_util::Name::new(format!("'{}", self.fresh_idx));
    self.fresh_idx += 1;
    ret
  }

  pub(crate) fn err(&mut self, range: TextRange, kind: ErrorKind) {
    self.errors.push(Error { range, kind });
  }

  pub(crate) fn finish(self, root: sml_hir::StrDecSeq) -> Lower {
    Lower { errors: self.errors, arenas: self.arenas, ptrs: self.ptrs, root }
  }

  pub(crate) fn str_dec(&mut self, val: sml_hir::StrDec, ptr: SyntaxNodePtr) -> sml_hir::StrDecIdx {
    let idx = self.arenas.str_dec.alloc(val);
    self.ptrs.insert(idx.into(), ptr);
    idx
  }

  pub(crate) fn str_exp(&mut self, val: sml_hir::StrExp, ptr: SyntaxNodePtr) -> sml_hir::StrExpIdx {
    let idx = self.arenas.str_exp.alloc(val);
    self.ptrs.insert(idx.into(), ptr);
    Some(idx)
  }

  pub(crate) fn sig_exp(&mut self, val: sml_hir::SigExp, ptr: SyntaxNodePtr) -> sml_hir::SigExpIdx {
    let idx = self.arenas.sig_exp.alloc(val);
    self.ptrs.insert(idx.into(), ptr);
    Some(idx)
  }

  pub(crate) fn spec(&mut self, val: sml_hir::Spec, ptr: SyntaxNodePtr) -> sml_hir::SpecIdx {
    let idx = self.arenas.spec.alloc(val);
    self.ptrs.insert(idx.into(), ptr);
    idx
  }

  pub(crate) fn exp(&mut self, val: sml_hir::Exp, ptr: SyntaxNodePtr) -> sml_hir::ExpIdx {
    let idx = self.arenas.exp.alloc(val);
    self.ptrs.insert(idx.into(), ptr);
    Some(idx)
  }

  pub(crate) fn dec(&mut self, val: sml_hir::Dec, ptr: SyntaxNodePtr) -> sml_hir::DecIdx {
    let idx = self.arenas.dec.alloc(val);
    self.ptrs.insert(idx.into(), ptr);
    idx
  }

  pub(crate) fn pat(&mut self, val: sml_hir::Pat, ptr: SyntaxNodePtr) -> sml_hir::PatIdx {
    let idx = self.arenas.pat.alloc(val);
    self.ptrs.insert(idx.into(), ptr);
    Some(idx)
  }

  pub(crate) fn ty(&mut self, val: sml_hir::Ty, ptr: SyntaxNodePtr) -> sml_hir::TyIdx {
    let idx = self.arenas.ty.alloc(val);
    self.ptrs.insert(idx.into(), ptr);
    Some(idx)
  }

  pub(crate) fn push_fun_name(&mut self, name: str_util::Name) {
    self.fun_names.push(name);
  }

  pub(crate) fn pop_fun_name(&mut self) {
    self.fun_names.pop().expect("no fun name to pop");
  }

  pub(crate) fn is_name_of_cur_fun(&self, name: &str) -> bool {
    self.fun_names.iter().any(|x| x.as_str() == name)
  }

  pub(crate) fn lang(&self) -> &'a Language {
    self.lang
  }

  pub(crate) fn inc_level(&mut self) {
    self.level += 1;
  }

  pub(crate) fn dec_level(&mut self) {
    self.level = self.level.checked_sub(1).expect("already at top level");
  }

  pub(crate) fn is_top_level(&self) -> bool {
    self.level == 0
  }
}
