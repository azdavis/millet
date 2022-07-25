use crate::fmt_util::{comma_seq, sep_seq};
use crate::pat_match::{Con, Pat, VariantName};
use crate::types::{MetaTyVar, MetaVarInfo, MetaVarNames, Sym, Syms, Ty};
use pattern_match::RawPat;
use std::fmt;

/// A statics error.
#[derive(Debug)]
pub struct Error {
  pub(crate) idx: hir::Idx,
  pub(crate) kind: ErrorKind,
}

impl Error {
  /// Returns the [`hir::Idx`] for this.
  pub fn idx(&self) -> hir::Idx {
    self.idx
  }

  /// Returns a value that displays the message.
  pub fn display<'a>(
    &'a self,
    syms: &'a Syms,
    mv_info: &'a MetaVarInfo,
    lines: config::ErrorLines,
  ) -> impl fmt::Display + 'a {
    ErrorKindDisplay {
      kind: &self.kind,
      syms,
      mv_info,
      lines,
    }
  }

  /// Return the code for this.
  pub fn to_code(&self) -> u8 {
    match self.kind {
      ErrorKind::Unsupported(_) => 1,
      ErrorKind::Undefined(_, _) => 2,
      ErrorKind::Duplicate(_, _) => 3,
      ErrorKind::Missing(_, _) => 4,
      ErrorKind::Extra(_, _) => 5,
      ErrorKind::Circularity(_, _) => 6,
      ErrorKind::MismatchedTypes(_, _) => 7,
      ErrorKind::AppLhsNotFn(_) => 8,
      ErrorKind::DuplicateLab(_) => 9,
      ErrorKind::RealPat => 10,
      ErrorKind::UnreachablePattern => 11,
      ErrorKind::NonExhaustiveCase(_) => 12,
      ErrorKind::NonExhaustiveBinding(_) => 13,
      ErrorKind::PatValIdStatus => 14,
      ErrorKind::ConPatMustNotHaveArg => 15,
      ErrorKind::ConPatMustHaveArg => 16,
      ErrorKind::InvalidAsPatName(_) => 17,
      ErrorKind::TyNameEscape(_) => 18,
      ErrorKind::ValRecExpNotFn => 19,
      ErrorKind::WrongNumTyArgs(_, _) => 20,
      ErrorKind::ExnCopyNotExnIdStatus => 21,
      ErrorKind::InvalidRebindName(_) => 22,
      ErrorKind::WrongIdStatus(_) => 23,
      ErrorKind::UnresolvedRecordTy => 24,
      ErrorKind::OrPatNotSameBindings(_) => 25,
      ErrorKind::DecNotAllowedHere => 26,
      ErrorKind::ExpHole(_) => 27,
      ErrorKind::TyHole => 28,
    }
  }
}

#[derive(Debug)]
pub(crate) enum ErrorKind {
  Unsupported(&'static str),
  Undefined(Item, hir::Name),
  Duplicate(Item, hir::Name),
  Missing(Item, hir::Name),
  Extra(Item, hir::Name),
  Circularity(MetaTyVar, Ty),
  MismatchedTypes(Ty, Ty),
  AppLhsNotFn(Ty),
  DuplicateLab(hir::Lab),
  RealPat,
  UnreachablePattern,
  NonExhaustiveCase(Vec<Pat>),
  NonExhaustiveBinding(Vec<Pat>),
  PatValIdStatus,
  ConPatMustNotHaveArg,
  ConPatMustHaveArg,
  InvalidAsPatName(hir::Name),
  TyNameEscape(Sym),
  ValRecExpNotFn,
  WrongNumTyArgs(usize, usize),
  ExnCopyNotExnIdStatus,
  InvalidRebindName(hir::Name),
  WrongIdStatus(hir::Name),
  UnresolvedRecordTy,
  OrPatNotSameBindings(hir::Name),
  DecNotAllowedHere,
  ExpHole(Ty),
  TyHole,
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
      ErrorKind::Unsupported(s) => write!(f, "unsupported language construct: {s}"),
      ErrorKind::Undefined(item, name) => write!(f, "undefined {item}: {name}"),
      ErrorKind::Duplicate(item, name) => write!(f, "duplicate {item}: {name}"),
      ErrorKind::Missing(item, name) => write!(f, "missing {item} required by signature: {name}"),
      ErrorKind::Extra(item, name) => write!(f, "extra {item} not present in signature: {name}"),
      ErrorKind::Circularity(mv, ty) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(ty);
        let name = mvs.get(mv).ok_or(fmt::Error)?;
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
        mismatched_types(f, self.lines, want, got)
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
      ErrorKind::TyNameEscape(sym) => write!(
        f,
        "type name escapes its scope: {}",
        self.syms.get(sym).unwrap().0
      ),
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
        write!(
          f,
          "{name} was bound in one or pattern alternative, but not in another"
        )
      }
      ErrorKind::DecNotAllowedHere => f.write_str("declaration not allowed here"),
      ErrorKind::ExpHole(ty) => {
        let mut mvs = MetaVarNames::new(self.mv_info);
        mvs.extend_for(ty);
        let ty = ty.display(&mvs, self.syms);
        write!(f, "expression hole with type {ty}")
      }
      ErrorKind::TyHole => f.write_str("type hole"),
    }
  }
}

fn non_exhaustive(
  f: &mut fmt::Formatter<'_>,
  syms: &Syms,
  pats: &[Pat],
  kind: &str,
) -> fmt::Result {
  write!(f, "non-exhaustive {kind}: missing ")?;
  assert!(!pats.is_empty());
  let max_len = 2;
  let iter = pats.iter().take(max_len).map(|pat| PatDisplay {
    pat,
    syms,
    prec: PatPrec::Min,
  });
  comma_seq(f, iter)?;
  if pats.len() > max_len {
    write!(f, ", and {} others", pats.len() - max_len)?;
  }
  Ok(())
}

fn mismatched_types<T>(
  f: &mut fmt::Formatter<'_>,
  lines: config::ErrorLines,
  want: T,
  got: T,
) -> fmt::Result
where
  T: fmt::Display,
{
  match lines {
    config::ErrorLines::One => write!(f, "expected {want}, found {got}"),
    config::ErrorLines::Many => {
      writeln!(f, "mismatched types:")?;
      writeln!(f, "  expected {want}")?;
      write!(f, "     found {got}")
    }
  }
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
          f.write_str("_")?
        }
        Con::Int(i) => {
          assert!(args.is_empty());
          write!(f, "{i}")?
        }
        Con::Word(w) => {
          assert!(args.is_empty());
          write!(f, "0w{w}")?
        }
        Con::Char(c) => {
          assert!(args.is_empty());
          write!(f, "#\"{c}\"")?
        }
        Con::String(s) => {
          assert!(args.is_empty());
          f.write_str(s.as_str())?
        }
        Con::Record {
          labels,
          allows_other,
        } => {
          assert_eq!(labels.len(), args.len());
          let is_tuple = !*allows_other
            && labels
              .iter()
              .enumerate()
              .all(|(idx, lab)| hir::Lab::tuple(idx) == *lab);
          if is_tuple {
            f.write_str("(")?;
            comma_seq(
              f,
              args.iter().map(|pat| PatDisplay {
                pat,
                syms: self.syms,
                prec: PatPrec::Min,
              }),
            )?;
            f.write_str(")")?;
          } else {
            f.write_str("{")?;
            comma_seq(
              f,
              labels
                .iter()
                .zip(args)
                .map(|(lab, pat)| RowDisplay::Row {
                  lab,
                  pat,
                  syms: self.syms,
                })
                .chain(allows_other.then_some(RowDisplay::Rest)),
            )?;
            f.write_str("}")?;
          }
        }
        Con::Variant(_, name) => {
          let name = match name {
            VariantName::Name(name) => name.as_str(),
            VariantName::Exn(exn) => self.syms.get_exn(exn).0.as_str(),
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
                  ac.into_iter().map(|pat| PatDisplay {
                    pat,
                    syms: self.syms,
                    prec: PatPrec::Min,
                  }),
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
                  ac.into_iter().map(|pat| PatDisplay {
                    pat,
                    syms: self.syms,
                    prec: PatPrec::App,
                  }),
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
                args.iter().map(|pat| PatDisplay {
                  pat,
                  syms: self.syms,
                  prec: PatPrec::App,
                }),
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
          pats.iter().map(|pat| PatDisplay {
            pat,
            syms: self.syms,
            prec: PatPrec::Min,
          }),
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
        Con::Record {
          allows_other: false,
          labels,
        } => labels,
        _ => unreachable!("the argument to :: is a record that does not allow others"),
      };
      assert!(
        labels.len() == 2 && (0..2).all(|x| labels.contains(&hir::Lab::tuple(x))),
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
  Row {
    lab: &'a hir::Lab,
    pat: &'a Pat,
    syms: &'a Syms,
  },
  Rest,
}

impl<'a> fmt::Display for RowDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      RowDisplay::Row { lab, pat, syms } => {
        lab.fmt(f)?;
        f.write_str(" = ")?;
        let pd = PatDisplay {
          pat,
          syms,
          prec: PatPrec::Min,
        };
        pd.fmt(f)
      }
      RowDisplay::Rest => f.write_str("..."),
    }
  }
}
