use crate::pat_match::{Con, Pat, VariantName};
use crate::types::{MetaTyVar, MetaVarInfo, MetaVarNames, Syms, Ty, TyScheme};
use diagnostic_util::{Code, Severity};
use fmt_util::{comma_seq, sep_seq};
use pattern_match::RawPat;
use std::fmt;

#[derive(Debug)]
pub(crate) enum ErrorKind {
  Undefined(Item, str_util::Name),
  Duplicate(Item, str_util::Name),
  Missing(Item, str_util::Name),
  Extra(Item, str_util::Name),
  Circularity(MetaTyVar, Ty),
  MismatchedTypes(Ty, Ty),
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
  ExnCopyNotExnIdStatus,
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
  /// must be last
  Unsupported(&'static str),
  InvalidAppend(AppendArg),
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
      ErrorKind::Undefined(_, _) => Code::n(5001),
      ErrorKind::Duplicate(_, _) => Code::n(5002),
      ErrorKind::Missing(_, _) => Code::n(5003),
      ErrorKind::Extra(_, _) => Code::n(5004),
      ErrorKind::Circularity(_, _) => Code::n(5005),
      ErrorKind::MismatchedTypes(_, _) => Code::n(5006),
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
      ErrorKind::ExnCopyNotExnIdStatus => Code::n(5020),
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
      ErrorKind::Unsupported(_) => Code::n(5999),
    }
  }

  /// Returns the severity for this.
  #[must_use]
  pub fn severity(&self) -> Severity {
    match self.kind {
      ErrorKind::Unused(_) | ErrorKind::InvalidEq(_) | ErrorKind::MismatchedFunctorSugar(_) => {
        Severity::Warning
      }
      _ => Severity::Error,
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
      AppendArg::Empty => f.write_str("empty"),
      AppendArg::Singleton => f.write_str("singleton"),
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

struct ErrorKindDisplay<'a> {
  kind: &'a ErrorKind,
  syms: &'a Syms,
  mv_info: &'a MetaVarInfo,
  lines: config::ErrorLines,
}

impl fmt::Display for ErrorKindDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.kind {
      ErrorKind::Undefined(item, name) => {
        write!(f, "undefined {item}: {name}")?;
        if let Some(sug) = suggestion(name.as_str()) {
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
        write!(f, "attempted to a set a type variable {name} ")?;
        write!(f, "to a type containing that variable: {ty}")
      }
      ErrorKind::MismatchedTypes(want, got) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(want);
        mvs.extend_for(got);
        let want = want.display(&mvs, self.syms);
        let got = got.display(&mvs, self.syms);
        match self.lines {
          config::ErrorLines::One => write!(f, "expected {want}, found {got}"),
          config::ErrorLines::Many => {
            writeln!(f, "mismatched types:")?;
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
      ErrorKind::NonExhaustiveCase(pats) => non_exhaustive(f, self.syms, pats, "case"),
      ErrorKind::NonExhaustiveBinding(pats) => non_exhaustive(f, self.syms, pats, "binding"),
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
      ErrorKind::ExnCopyNotExnIdStatus => f.write_str("not an exception"),
      ErrorKind::InvalidRebindName(name) => write!(f, "cannot re-bind name: {name}"),
      ErrorKind::WrongIdStatus(name) => write!(f, "incompatible identifier statuses: {name}"),
      ErrorKind::UnresolvedRecordTy => {
        f.write_str("cannot resolve record type containing `...` due to lack of context")
      }
      ErrorKind::OrPatNotSameBindings(name) => {
        write!(f, "{name} was bound in one or pattern alternative, but not in another")
      }
      ErrorKind::DecNotAllowedHere => {
        f.write_str("`signature` or `functor` declaration not allowed here")
      }
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
      ErrorKind::InvalidAppend(kind) => write!(f, "calling `@` with a {kind} list"),
      ErrorKind::Unsupported(s) => write!(f, "unsupported language construct: {s}"),
    }
  }
}

fn suggestion(s: &str) -> Option<&'static str> {
  let ret = match s {
    "func" | "function" | "def" => "fun",
    "lambda" => "fn",
    "const" | "var" => "val",
    "enum" => "datatype",
    "begin" => "local",
    "module" | "namespace" => "structure",
    "match" | "switch" => "case",
    "elsif" | "elif" => "else if",
    "integer" => "int",
    "Integer" => "Int",
    "boolean" => "bool",
    "Boolean" => "Bool",
    "character" | "rune" => "char",
    "Character" | "Rune" => "Char",
    "uint" | "unsigned" => "word",
    "Uint" | "UInt" | "Unsigned" => "Word",
    "void" => "unit",
    "float" | "double" => "real",
    "Float" | "Double" => "Real",
    "str" => "string",
    "Str" => "String",
    "vec" => "vector",
    "Vec" => "Vector",
    "Optional" | "Maybe" => "option",
    // nil is the empty list
    "none" | "None" | "Nothing" | "null" | "NULL" | "nullptr" | "undefined" => "NONE",
    "some" | "Some" | "Just" => "SOME",
    "True" | "TRUE" | "YES" => "true",
    "False" | "FALSE" | "NO" => "false",
    _ => return None,
  };
  Some(ret)
}

fn non_exhaustive(
  f: &mut fmt::Formatter<'_>,
  syms: &Syms,
  pats: &[Pat],
  kind: &str,
) -> fmt::Result {
  write!(f, "non-exhaustive {kind}: missing ")?;
  assert!(!pats.is_empty());
  let max_len = 3usize;
  let iter = pats.iter().take(max_len).map(|pat| PatDisplay { pat, syms, prec: PatPrec::Min });
  comma_seq(f, iter)?;
  if let Some(n) = pats.len().checked_sub(max_len) {
    if n != 0 {
      write!(f, ", and {n} others")?;
    }
  }
  Ok(())
}

struct PatDisplay<'a> {
  pat: &'a Pat,
  syms: &'a Syms,
  prec: PatPrec,
}

impl<'a> fmt::Display for PatDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.pat.raw {
      RawPat::Con(con, args) => match con {
        Con::Any => {
          assert!(args.is_empty());
          f.write_str("_")?;
        }
        Con::Int(i) => {
          assert!(args.is_empty());
          write!(f, "{i}")?;
        }
        Con::Word(w) => {
          assert!(args.is_empty());
          write!(f, "0w{w}")?;
        }
        Con::Char(c) => {
          assert!(args.is_empty());
          write!(f, "#\"{c}\"")?;
        }
        Con::String(s) => {
          assert!(args.is_empty());
          f.write_str(s.as_str())?;
        }
        Con::Record { labels, allows_other } => {
          assert_eq!(labels.len(), args.len());
          let is_tuple = !*allows_other
            && labels.iter().enumerate().all(|(idx, lab)| sml_hir::Lab::tuple(idx) == *lab);
          if is_tuple {
            f.write_str("(")?;
            comma_seq(
              f,
              args.iter().map(|pat| PatDisplay { pat, syms: self.syms, prec: PatPrec::Min }),
            )?;
            f.write_str(")")?;
          } else {
            f.write_str("{")?;
            comma_seq(
              f,
              labels
                .iter()
                .zip(args)
                .map(|(lab, pat)| RowDisplay::Row { lab, pat, syms: self.syms })
                .chain(allows_other.then_some(RowDisplay::Rest)),
            )?;
            f.write_str("}")?;
          }
        }
        Con::Variant(_, name) => {
          let name = match name {
            VariantName::Name(name) => name.as_str(),
            VariantName::Exn(exn) => self.syms.get_exn(*exn).0.last().as_str(),
          };
          let needs_paren = !args.is_empty() && matches!(self.prec, PatPrec::App);
          // these names are guaranteed not to be rebound, so they always are list constructors.
          if matches!(name, "nil" | "::") {
            let mut ac = Vec::new();
            match list_pat(&mut ac, self.pat) {
              // does not need paren because list literal patterns are atomic
              ListPatLen::Known => {
                f.write_str("[")?;
                comma_seq(
                  f,
                  ac.into_iter().map(|pat| PatDisplay { pat, syms: self.syms, prec: PatPrec::Min }),
                )?;
                f.write_str("]")?;
              }
              ListPatLen::Unknown => {
                if needs_paren {
                  f.write_str("(")?;
                }
                sep_seq(
                  f,
                  " :: ",
                  ac.into_iter().map(|pat| PatDisplay { pat, syms: self.syms, prec: PatPrec::App }),
                )?;
                if needs_paren {
                  f.write_str(")")?;
                }
              }
            }
          } else {
            if needs_paren {
              f.write_str("(")?;
            }
            f.write_str(name)?;
            if !args.is_empty() {
              f.write_str(" ")?;
              comma_seq(
                f,
                args.iter().map(|pat| PatDisplay { pat, syms: self.syms, prec: PatPrec::App }),
              )?;
            }
            if needs_paren {
              f.write_str(")")?;
            }
          }
        }
      },
      RawPat::Or(pats) => {
        f.write_str("(")?;
        sep_seq(
          f,
          " | ",
          pats.iter().map(|pat| PatDisplay { pat, syms: self.syms, prec: PatPrec::Min }),
        )?;
        f.write_str(")")?;
      }
    }
    Ok(())
  }
}

enum ListPatLen {
  Known,
  Unknown,
}

fn list_pat<'p>(ac: &mut Vec<&'p Pat>, pat: &'p Pat) -> ListPatLen {
  let (con, args) = match &pat.raw {
    RawPat::Con(a, b) => (a, b),
    RawPat::Or(_) => {
      ac.push(pat);
      return ListPatLen::Unknown;
    }
  };
  let name = match con {
    Con::Any => {
      ac.push(pat);
      return ListPatLen::Unknown;
    }
    Con::Variant(_, VariantName::Name(name)) => name.as_str(),
    _ => unreachable!("only Any and Variant cons can have list type"),
  };
  match name {
    "nil" => {
      assert!(args.is_empty());
      ListPatLen::Known
    }
    "::" => {
      assert_eq!(args.len(), 1, ":: has an argument");
      let (con, args) = match &args.first().unwrap().raw {
        RawPat::Con(a, b) => (a, b),
        RawPat::Or(_) => unreachable!("the argument to :: is a con pat"),
      };
      let labels = match con {
        Con::Record { allows_other: false, labels } => labels,
        _ => unreachable!("the argument to :: is a record that does not allow others"),
      };
      assert!(
        labels.len() == 2 && (0..2).all(|x| labels.contains(&sml_hir::Lab::tuple(x))),
        "the argument to :: is a 2-tuple"
      );
      let (hd, tl) = match &args[..] {
        [a, b] => (a, b),
        _ => unreachable!("the argument to :: is a 2-tuple"),
      };
      ac.push(hd);
      list_pat(ac, tl)
    }
    _ => unreachable!("the list constructors are nil and ::"),
  }
}

enum PatPrec {
  Min,
  App,
}

enum RowDisplay<'a> {
  Row { lab: &'a sml_hir::Lab, pat: &'a Pat, syms: &'a Syms },
  Rest,
}

impl<'a> fmt::Display for RowDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      RowDisplay::Row { lab, pat, syms } => {
        lab.fmt(f)?;
        f.write_str(" = ")?;
        let pd = PatDisplay { pat, syms, prec: PatPrec::Min };
        pd.fmt(f)
      }
      RowDisplay::Rest => f.write_str("..."),
    }
  }
}
