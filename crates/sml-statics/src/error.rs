//! Errors.

mod non_exhaustive;
mod suggestion;

use crate::pat_match::Pat;
use diagnostic::{Code, Severity};
use sml_statics_types::display::record_meta_var;
use sml_statics_types::ty::{RecordData, Ty, TyScheme};
use sml_statics_types::unify::{Circularity, Incompatible};
use sml_statics_types::{disallow::Disallow, item::Item};
use std::fmt;

#[derive(Debug)]
pub(crate) enum ErrorKind {
  /// must be first here, but have the highest error code
  Unsupported(&'static str),
  Undefined(Item, str_util::Name),
  Duplicate(Item, str_util::Name),
  Missing(Item, str_util::Name),
  Extra(Item, str_util::Name),
  Circularity(Circularity),
  IncompatibleTys(Incompatible, Ty, Ty),
  DuplicateLab(sml_hir::Lab),
  RealPat,
  UnreachablePattern,
  NonExhaustiveCase(Vec<Pat>),
  NonExhaustiveBinding(Vec<Pat>),
  PatValIdStatus,
  ConPatMustNotHaveArg,
  ConPatMustHaveArg,
  InvalidAsPatName(str_util::Name),
  TyEscape(Ty),
  ValRecExpNotFn,
  WrongNumTyArgs(usize, usize),
  ExnCopyNotExnIdStatus(sml_path::Path),
  InvalidRebindName(str_util::Name),
  WrongIdStatus(str_util::Name),
  UnresolvedRecordTy(RecordData),
  OrPatNotSameBindings(str_util::Name),
  DecNotAllowedHere,
  ExpHole(Ty),
  TyHole,
  BindPolymorphicExpansiveExp,
  Unused(Item, str_util::Name),
  TyVarNotAllowedForTyRhs,
  CannotShareTy(sml_path::Path, TyScheme),
  CannotRealizeTy(sml_path::Path, TyScheme),
  InvalidEq(str_util::Name),
  /// The argument is the more sugary one.
  MismatchedFunctorSugar(FunctorSugarUser),
  InvalidAppend(AppendArg),
  BoolCase,
  AppFn,
  Use(Option<str_util::SmolStr>),
  UnreachableHandle,
  DecWithoutEffect,
  Disallowed(Item, Disallow, str_util::Name),
  CanEtaReduce(str_util::Name),
  ShadowInCaseWithSameTy(str_util::Name, TyScheme),
}

struct ErrorKindDisplay<'a> {
  kind: &'a ErrorKind,
  st: &'a sml_statics_types::St,
  lines: config::DiagnosticLines,
}

impl fmt::Display for ErrorKindDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.kind {
      ErrorKind::Unsupported(s) => write!(f, "unsupported: {s}"),
      ErrorKind::Undefined(item, name) => {
        write!(f, "undefined {item}: `{name}`")?;
        if let Some(sug) = suggestion::get(name.as_str()) {
          write!(f, " (did you mean `{sug}`?)")?;
        }
        Ok(())
      }
      ErrorKind::Duplicate(item, name) => write!(f, "duplicate {item}: `{name}`"),
      ErrorKind::Missing(item, name) => write!(f, "missing {item} required by signature: `{name}`"),
      ErrorKind::Extra(item, name) => write!(f, "extra {item} not present in signature: `{name}`"),
      ErrorKind::Circularity(circ) => {
        let mv = circ.meta_var.display(self.st, self.lines);
        let ty = circ.ty.display(self.st, self.lines);
        write!(f, "circular type: `{mv}` occurs in `{ty}`")
      }
      ErrorKind::IncompatibleTys(reason, want, got) => {
        let reason = reason.display(self.st, self.lines);
        write!(f, "incompatible types: {reason}")?;
        let want = want.display(self.st, self.lines);
        let got = got.display(self.st, self.lines);
        match self.lines {
          config::DiagnosticLines::One => {
            write!(f, ": expected `{want}`, found `{got}`")
          }
          config::DiagnosticLines::Many => {
            writeln!(f, "\n  expected `{want}`")?;
            write!(f, "     found `{got}`")
          }
        }
      }
      ErrorKind::DuplicateLab(lab) => write!(f, "duplicate label: `{lab}`"),
      ErrorKind::RealPat => f.write_str("real literal used as a pattern"),
      ErrorKind::UnreachablePattern => f.write_str("unreachable pattern"),
      ErrorKind::NonExhaustiveCase(pats) => non_exhaustive::get(f, &self.st.syms, pats, "case"),
      ErrorKind::NonExhaustiveBinding(pats) => {
        non_exhaustive::get(f, &self.st.syms, pats, "binding")
      }
      ErrorKind::PatValIdStatus => f.write_str("value binding used as a pattern"),
      ErrorKind::ConPatMustNotHaveArg => f.write_str("unexpected argument for constructor pattern"),
      ErrorKind::ConPatMustHaveArg => f.write_str("missing argument for constructor pattern"),
      ErrorKind::InvalidAsPatName(name) => write!(f, "invalid `as` pat name: `{name}`"),
      ErrorKind::TyEscape(ty) => {
        let ty = ty.display(self.st, self.lines);
        write!(f, "type escapes its scope: `{ty}`")
      }
      ErrorKind::ValRecExpNotFn => f.write_str("the expression for a `val rec` was not a `fn`"),
      ErrorKind::WrongNumTyArgs(want, got) => {
        let s = if *want == 1 { "" } else { "s" };
        write!(f, "expected {want} type argument{s}, found {got}")
      }
      ErrorKind::ExnCopyNotExnIdStatus(path) => write!(f, "not an exception: `{path}`"),
      ErrorKind::InvalidRebindName(name) => write!(f, "cannot re-bind name: `{name}`"),
      ErrorKind::WrongIdStatus(name) => write!(f, "incompatible identifier statuses: `{name}`"),
      ErrorKind::UnresolvedRecordTy(rows) => {
        let ty = record_meta_var(self.st, rows, self.lines);
        write!(f, "cannot resolve `...` in record type: `{ty}`")
      }
      ErrorKind::OrPatNotSameBindings(name) => {
        write!(f, "`{name}` was bound in one alternative, but not in another")
      }
      ErrorKind::DecNotAllowedHere => f.write_str("`signature` or `functor` not allowed here"),
      ErrorKind::ExpHole(ty) => {
        let ty = ty.display(self.st, self.lines);
        write!(f, "expression hole with type `{ty}`")
      }
      ErrorKind::TyHole => f.write_str("type hole"),
      ErrorKind::BindPolymorphicExpansiveExp => {
        f.write_str("cannot bind expansive polymorphic expression")
      }
      ErrorKind::Unused(item, name) => {
        write!(f, "unused {item}: `{name}`")
      }
      ErrorKind::TyVarNotAllowedForTyRhs => {
        f.write_str("type variable bound at `val` or `fun` not allowed here")
      }
      ErrorKind::CannotShareTy(path, ts) => {
        let ts = ts.display(self.st, self.lines);
        write!(f, "cannot share type `{path}` as `{ts}`")
      }
      ErrorKind::CannotRealizeTy(path, ts) => {
        let ts = ts.display(self.st, self.lines);
        write!(f, "cannot realize type `{path}` as `{ts}`")
      }
      ErrorKind::InvalidEq(name) => write!(f, "calling `=` or `<>` on `{name}`"),
      ErrorKind::MismatchedFunctorSugar(sugary) => {
        let other = sugary.other();
        write!(f, "the {sugary} uses syntax sugar, but the {other} does not")
      }
      ErrorKind::InvalidAppend(kind) => write!(f, "calling `@` with {kind}"),
      ErrorKind::BoolCase => f.write_str("`case` on a `bool`"),
      ErrorKind::AppFn => f.write_str("applying a function literal to an argument"),
      ErrorKind::Use(file) => {
        f.write_str("`use` ignored, no additional definitions ")?;
        if let Some(file) = file {
          write!(f, "from {file:?} ")?;
        }
        f.write_str("brought into scope")
      }
      ErrorKind::UnreachableHandle => f.write_str("unreachable `handle`"),
      ErrorKind::DecWithoutEffect => f.write_str("declaration with no effect"),
      ErrorKind::Disallowed(item, d, name) => write!(f, "{d} disallowed {item}: `{name}`"),
      ErrorKind::CanEtaReduce(name) => {
        f.write_str("this `fn` expression, of the form ")?;
        write!(f, "`fn {name} => f {name}`, can be simplified to `f`")
      }
      ErrorKind::ShadowInCaseWithSameTy(name, ty_scheme) => {
        let ty_scheme = ty_scheme.display(self.st, self.lines);
        write!(f, "this pattern, which binds `{name}` with type `{ty_scheme}`, ")?;
        write!(f, "does not check any part of the matched expression for equality with the ")?;
        write!(f, "existing `{name}`, which has the same type, and which was already in scope")
      }
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum FunctorSugarUser {
  Def,
  App,
}

impl FunctorSugarUser {
  fn other(self) -> Self {
    match self {
      FunctorSugarUser::Def => FunctorSugarUser::App,
      FunctorSugarUser::App => FunctorSugarUser::Def,
    }
  }
}

impl fmt::Display for FunctorSugarUser {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      FunctorSugarUser::Def => f.write_str("`functor` definition"),
      FunctorSugarUser::App => f.write_str("`functor` application"),
    }
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum AppendArg {
  Empty,
  Singleton,
}

impl fmt::Display for AppendArg {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      AppendArg::Empty => f.write_str("an empty list"),
      AppendArg::Singleton => f.write_str("a singleton list"),
    }
  }
}

/// A statics error.
#[derive(Debug)]
pub struct Error {
  pub(crate) idx: sml_hir::Idx,
  pub(crate) kind: ErrorKind,
}

impl Error {
  /// Returns the [`sml_hir::Idx`] for this.
  #[must_use]
  pub fn idx(&self) -> sml_hir::Idx {
    self.idx
  }

  /// Returns a value that displays the message.
  #[must_use]
  pub fn display<'a>(
    &'a self,
    st: &'a sml_statics_types::St,
    lines: config::DiagnosticLines,
  ) -> impl fmt::Display + 'a {
    ErrorKindDisplay { kind: &self.kind, st, lines }
  }

  /// Return the code for this.
  ///
  /// No longer used:
  ///
  /// - `Code::n(5007)`
  #[must_use]
  pub fn code(&self) -> Code {
    match self.kind {
      ErrorKind::Unsupported(_) => Code::n(5999),
      ErrorKind::Undefined(_, _) => Code::n(5001),
      ErrorKind::Duplicate(_, _) => Code::n(5002),
      ErrorKind::Missing(_, _) => Code::n(5003),
      ErrorKind::Extra(_, _) => Code::n(5004),
      ErrorKind::Circularity(_) => Code::n(5005),
      ErrorKind::IncompatibleTys(_, _, _) => Code::n(5006),
      ErrorKind::DuplicateLab(_) => Code::n(5008),
      ErrorKind::RealPat => Code::n(5009),
      ErrorKind::UnreachablePattern => Code::n(5010),
      ErrorKind::NonExhaustiveCase(_) => Code::n(5011),
      ErrorKind::NonExhaustiveBinding(_) => Code::n(5012),
      ErrorKind::PatValIdStatus => Code::n(5013),
      ErrorKind::ConPatMustNotHaveArg => Code::n(5014),
      ErrorKind::ConPatMustHaveArg => Code::n(5015),
      ErrorKind::InvalidAsPatName(_) => Code::n(5016),
      ErrorKind::TyEscape(_) => Code::n(5017),
      ErrorKind::ValRecExpNotFn => Code::n(5018),
      ErrorKind::WrongNumTyArgs(_, _) => Code::n(5019),
      ErrorKind::ExnCopyNotExnIdStatus(_) => Code::n(5020),
      ErrorKind::InvalidRebindName(_) => Code::n(5021),
      ErrorKind::WrongIdStatus(_) => Code::n(5022),
      ErrorKind::UnresolvedRecordTy(_) => Code::n(5023),
      ErrorKind::OrPatNotSameBindings(_) => Code::n(5024),
      ErrorKind::DecNotAllowedHere => Code::n(5025),
      ErrorKind::ExpHole(_) => Code::n(5026),
      ErrorKind::TyHole => Code::n(5027),
      ErrorKind::BindPolymorphicExpansiveExp => Code::n(5028),
      ErrorKind::Unused(_, _) => Code::n(5029),
      ErrorKind::TyVarNotAllowedForTyRhs => Code::n(5030),
      ErrorKind::CannotShareTy(_, _) => Code::n(5031),
      ErrorKind::CannotRealizeTy(_, _) => Code::n(5032),
      ErrorKind::InvalidEq(_) => Code::n(5033),
      ErrorKind::MismatchedFunctorSugar(_) => Code::n(5034),
      ErrorKind::InvalidAppend(_) => Code::n(5035),
      ErrorKind::BoolCase => Code::n(5036),
      ErrorKind::AppFn => Code::n(5037),
      ErrorKind::Use(_) => Code::n(5038),
      ErrorKind::UnreachableHandle => Code::n(5039),
      ErrorKind::DecWithoutEffect => Code::n(5040),
      ErrorKind::Disallowed(_, _, _) => Code::n(5041),
      ErrorKind::CanEtaReduce(_) => Code::n(5042),
      ErrorKind::ShadowInCaseWithSameTy(_, _) => Code::n(5043),
    }
  }

  /// Returns the severity for this.
  #[must_use]
  pub fn severity(&self) -> Severity {
    match self.kind {
      ErrorKind::Unused(_, _)
      | ErrorKind::InvalidEq(_)
      | ErrorKind::MismatchedFunctorSugar(_)
      | ErrorKind::InvalidAppend(_)
      | ErrorKind::BoolCase
      | ErrorKind::AppFn
      | ErrorKind::Use(_)
      | ErrorKind::UnreachableHandle
      | ErrorKind::DecWithoutEffect
      | ErrorKind::CanEtaReduce(_)
      | ErrorKind::ShadowInCaseWithSameTy(_, _) => Severity::Warning,
      _ => Severity::Error,
    }
  }
}
