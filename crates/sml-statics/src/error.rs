mod non_exhaustive;
mod suggestion;

use crate::pat_match::Pat;
use crate::types::{
  BoundTyVar, FixedTyVar, MetaTyVar, MetaVarInfo, MetaVarNames, Overload, RecordTy, Sym,
  SymDisplay, Syms, Ty, TyScheme,
};
use diagnostic_util::{Code, Severity};
use std::fmt;

#[derive(Debug)]
pub(crate) enum ErrorKind {
  /// must be first here, but have the highest error code
  Unsupported(&'static str),
  Undefined(Item, str_util::Name),
  Duplicate(Item, str_util::Name),
  Missing(Item, str_util::Name),
  Extra(Item, str_util::Name),
  Circularity(MetaTyVar, Ty),
  MismatchedTypes(MismatchedTypesFlavor, Ty, Ty),
  AppLhsNotFn(Ty),
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
  ExnCopyNotExnIdStatus(sml_hir::Path),
  InvalidRebindName(str_util::Name),
  WrongIdStatus(str_util::Name),
  UnresolvedRecordTy,
  OrPatNotSameBindings(str_util::Name),
  DecNotAllowedHere,
  ExpHole(Ty),
  TyHole,
  BindPolymorphicExpansiveExp,
  Unused(str_util::Name),
  TyVarNotAllowedForTyRhs,
  CannotShareTy(sml_hir::Path, TyScheme),
  CannotRealizeTy(sml_hir::Path, TyScheme),
  InvalidEq(str_util::Name),
  /// The argument is the more sugary one.
  MismatchedFunctorSugar(FunctorSugarUser),
  InvalidAppend(AppendArg),
  BoolCase,
}

struct ErrorKindDisplay<'a> {
  kind: &'a ErrorKind,
  syms: &'a Syms,
  mv_info: &'a MetaVarInfo,
  lines: config::ErrorLines,
}

impl fmt::Display for ErrorKindDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.kind {
      ErrorKind::Unsupported(s) => write!(f, "unsupported: {s}"),
      ErrorKind::Undefined(item, name) => {
        write!(f, "undefined {item}: {name}")?;
        if let Some(sug) = suggestion::get(name.as_str()) {
          write!(f, " (did you mean `{sug}`?)")?;
        }
        Ok(())
      }
      ErrorKind::Duplicate(item, name) => write!(f, "duplicate {item}: {name}"),
      ErrorKind::Missing(item, name) => write!(f, "missing {item} required by signature: {name}"),
      ErrorKind::Extra(item, name) => write!(f, "extra {item} not present in signature: {name}"),
      ErrorKind::Circularity(mv, ty) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(ty);
        let name = mvs.get(*mv).ok_or(fmt::Error)?;
        let ty = ty.display(&mvs, self.syms);
        write!(f, "circular type: {name} occurs in {ty}")
      }
      ErrorKind::MismatchedTypes(flavor, want, got) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(want);
        mvs.extend_for(got);
        let want = want.display(&mvs, self.syms);
        let got = got.display(&mvs, self.syms);
        match self.lines {
          config::ErrorLines::One => write!(f, "expected {want}, found {got}"),
          config::ErrorLines::Many => {
            writeln!(f, "mismatched types: {}", flavor.display(&mvs, self.syms))?;
            writeln!(f, "  expected {want}")?;
            write!(f, "     found {got}")
          }
        }
      }
      ErrorKind::AppLhsNotFn(got) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(got);
        let got = got.display(&mvs, self.syms);
        write!(f, "expected a function type, found {got}")
      }
      ErrorKind::DuplicateLab(lab) => write!(f, "duplicate label: {lab}"),
      ErrorKind::RealPat => f.write_str("real literal used as a pattern"),
      ErrorKind::UnreachablePattern => f.write_str("unreachable pattern"),
      ErrorKind::NonExhaustiveCase(pats) => non_exhaustive::get(f, self.syms, pats, "case"),
      ErrorKind::NonExhaustiveBinding(pats) => non_exhaustive::get(f, self.syms, pats, "binding"),
      ErrorKind::PatValIdStatus => f.write_str("value binding used as a pattern"),
      ErrorKind::ConPatMustNotHaveArg => f.write_str("unexpected argument for constructor pattern"),
      ErrorKind::ConPatMustHaveArg => f.write_str("missing argument for constructor pattern"),
      ErrorKind::InvalidAsPatName(name) => write!(f, "invalid `as` pat name: {name}"),
      ErrorKind::TyEscape(ty) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(ty);
        let ty = ty.display(&mvs, self.syms);
        write!(f, "type escapes its scope: {ty}")
      }
      ErrorKind::ValRecExpNotFn => f.write_str("the expression for a `val rec` was not a `fn`"),
      ErrorKind::WrongNumTyArgs(want, got) => {
        let s = if *want == 1 { "" } else { "s" };
        write!(f, "expected {want} type argument{s}, found {got}")
      }
      ErrorKind::ExnCopyNotExnIdStatus(path) => write!(f, "not an exception: {path}"),
      ErrorKind::InvalidRebindName(name) => write!(f, "cannot re-bind name: {name}"),
      ErrorKind::WrongIdStatus(name) => write!(f, "incompatible identifier statuses: {name}"),
      ErrorKind::UnresolvedRecordTy => f.write_str("cannot resolve record type containing `...`"),
      ErrorKind::OrPatNotSameBindings(name) => {
        write!(f, "{name} was bound in one alternative, but not in another")
      }
      ErrorKind::DecNotAllowedHere => f.write_str("`signature` or `functor` not allowed here"),
      ErrorKind::ExpHole(ty) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(ty);
        let ty = ty.display(&mvs, self.syms);
        write!(f, "expression hole with type {ty}")
      }
      ErrorKind::TyHole => f.write_str("type hole"),
      ErrorKind::BindPolymorphicExpansiveExp => {
        f.write_str("cannot bind expansive polymorphic expression")
      }
      ErrorKind::Unused(name) => {
        let item = Item::Val;
        write!(f, "unused {item}: {name}")
      }
      ErrorKind::TyVarNotAllowedForTyRhs => {
        f.write_str("type variable bound at `val` or `fun` not allowed here")
      }
      ErrorKind::CannotShareTy(path, ts) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(&ts.ty);
        let ts = ts.display(&mvs, self.syms);
        write!(f, "cannot share type {path} as {ts}")
      }
      ErrorKind::CannotRealizeTy(path, ts) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(&ts.ty);
        let ts = ts.display(&mvs, self.syms);
        write!(f, "cannot realize type {path} as {ts}")
      }
      ErrorKind::InvalidEq(name) => write!(f, "calling `=` or `<>` on {name}"),
      ErrorKind::MismatchedFunctorSugar(sugary) => {
        let other = sugary.other();
        write!(f, "the {sugary} uses syntax sugar, but the {other} does not")
      }
      ErrorKind::InvalidAppend(kind) => write!(f, "calling `@` with {kind}"),
      ErrorKind::BoolCase => f.write_str("`case` on a `bool`"),
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
      FunctorSugarUser::Def => f.write_str("functor definition"),
      FunctorSugarUser::App => f.write_str("functor application"),
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

#[derive(Debug)]
pub(crate) enum Item {
  Val,
  Ty,
  TyVar,
  Struct,
  Sig,
  Functor,
}

impl fmt::Display for Item {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Item::Val => f.write_str("value"),
      Item::Ty => f.write_str("type"),
      Item::TyVar => f.write_str("type variable"),
      Item::Struct => f.write_str("structure"),
      Item::Sig => f.write_str("signature"),
      Item::Functor => f.write_str("functor"),
    }
  }
}

#[derive(Debug)]
pub(crate) enum MismatchedTypesFlavor {
  /// NOTE: this might never happen.
  BoundTyVar(BoundTyVar, BoundTyVar),
  FixedTyVar(FixedTyVar, FixedTyVar),
  MissingRow(sml_hir::Lab),
  ExtraRows(RecordTy),
  Con(Sym, Sym),
  Head(Ty, Ty),
  OverloadCon(Overload, Sym),
  OverloadUnify(Overload, Overload),
  OverloadRecord(RecordTy, Overload),
  OverloadHeadMismatch(Overload, Ty),
  UnresolvedRecordMissingRow(sml_hir::Lab),
  UnresolvedRecordHeadMismatch(RecordTy, Ty),
}

impl MismatchedTypesFlavor {
  fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
  ) -> MismatchedTypesFlavorDisplay<'a> {
    MismatchedTypesFlavorDisplay { flavor: self, meta_vars, syms }
  }
}

struct MismatchedTypesFlavorDisplay<'a> {
  flavor: &'a MismatchedTypesFlavor,
  meta_vars: &'a MetaVarNames<'a>,
  syms: &'a Syms,
}

impl fmt::Display for MismatchedTypesFlavorDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.flavor {
      MismatchedTypesFlavor::BoundTyVar(_, _) => f.write_str("type variables are different"),
      MismatchedTypesFlavor::FixedTyVar(a, b) => {
        write!(f, "{a} and {b} are different type variables")
      }
      MismatchedTypesFlavor::MissingRow(lab) => {
        write!(f, "record or tuple type is missing field: {lab}")
      }
      MismatchedTypesFlavor::ExtraRows(rows) => {
        write!(f, "record or tuple type has extra fields: ")?;
        fmt_util::comma_seq(f, rows.iter().map(|(lab, _)| lab))
      }
      MismatchedTypesFlavor::Con(a, b) => {
        let a = SymDisplay { sym: *a, syms: self.syms };
        let b = SymDisplay { sym: *b, syms: self.syms };
        write!(f, "{a} and {b} are different type constructors")
      }
      MismatchedTypesFlavor::Head(a, b) => {
        let a_display = a.display(self.meta_vars, self.syms);
        let b_display = b.display(self.meta_vars, self.syms);
        let a_desc = a.desc();
        let b_desc = b.desc();
        write!(f, "{a_display} is {a_desc}, but {b_display} is {b_desc}")
      }
      MismatchedTypesFlavor::OverloadCon(ov, s) => {
        let s = SymDisplay { sym: *s, syms: self.syms };
        write!(f, "{s} is not compatible with the {ov} overload")
      }
      MismatchedTypesFlavor::OverloadUnify(want, got) => {
        write!(f, "{want} and {got} are incompatible overloads")
      }
      MismatchedTypesFlavor::OverloadRecord(_, ov) => {
        write!(f, "record types is not compatible with the {ov} overload")
      }
      MismatchedTypesFlavor::OverloadHeadMismatch(ov, ty) => {
        let ty_display = ty.display(self.meta_vars, self.syms);
        let ty_desc = ty.desc();
        write!(f, "{ov} is not compatible with {ty_display}, which is {ty_desc}")
      }
      MismatchedTypesFlavor::UnresolvedRecordMissingRow(lab) => {
        write!(f, "unresolved record or tuple type is missing field: {lab}")
      }
      MismatchedTypesFlavor::UnresolvedRecordHeadMismatch(_, ty) => {
        let ty_display = ty.display(self.meta_vars, self.syms);
        let ty_desc = ty.desc();
        write!(
          f,
          "unresolved record or tuple type is not compatible with {ty_display}, which is {ty_desc}"
        )
      }
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
    syms: &'a Syms,
    mv_info: &'a MetaVarInfo,
    lines: config::ErrorLines,
  ) -> impl fmt::Display + 'a {
    ErrorKindDisplay { kind: &self.kind, syms, mv_info, lines }
  }

  /// Return the code for this.
  #[must_use]
  pub fn code(&self) -> Code {
    match self.kind {
      ErrorKind::Unsupported(_) => Code::n(5999),
      ErrorKind::Undefined(_, _) => Code::n(5001),
      ErrorKind::Duplicate(_, _) => Code::n(5002),
      ErrorKind::Missing(_, _) => Code::n(5003),
      ErrorKind::Extra(_, _) => Code::n(5004),
      ErrorKind::Circularity(_, _) => Code::n(5005),
      ErrorKind::MismatchedTypes(_, _, _) => Code::n(5006),
      ErrorKind::AppLhsNotFn(_) => Code::n(5007),
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
      ErrorKind::UnresolvedRecordTy => Code::n(5023),
      ErrorKind::OrPatNotSameBindings(_) => Code::n(5024),
      ErrorKind::DecNotAllowedHere => Code::n(5025),
      ErrorKind::ExpHole(_) => Code::n(5026),
      ErrorKind::TyHole => Code::n(5027),
      ErrorKind::BindPolymorphicExpansiveExp => Code::n(5028),
      ErrorKind::Unused(_) => Code::n(5029),
      ErrorKind::TyVarNotAllowedForTyRhs => Code::n(5030),
      ErrorKind::CannotShareTy(_, _) => Code::n(5031),
      ErrorKind::CannotRealizeTy(_, _) => Code::n(5032),
      ErrorKind::InvalidEq(_) => Code::n(5033),
      ErrorKind::MismatchedFunctorSugar(_) => Code::n(5034),
      ErrorKind::InvalidAppend(_) => Code::n(5035),
      ErrorKind::BoolCase => Code::n(5036),
    }
  }

  /// Returns the severity for this.
  #[must_use]
  pub fn severity(&self) -> Severity {
    match self.kind {
      ErrorKind::Unused(_)
      | ErrorKind::InvalidEq(_)
      | ErrorKind::MismatchedFunctorSugar(_)
      | ErrorKind::BoolCase => Severity::Warning,
      _ => Severity::Error,
    }
  }
}
