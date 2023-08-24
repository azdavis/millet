//! Displaying non-exhaustive errors with witnesses.

use crate::pat_match::{Con, Lang, Pat, VariantName};
use fmt_util::{comma_seq, sep_seq};
use pattern_match::{ConPat, RawPat};
use sml_statics_types::sym::Syms;
use std::fmt;

pub(crate) fn get(
  f: &mut fmt::Formatter<'_>,
  syms: &Syms,
  pats: &[Pat],
  kind: &str,
) -> fmt::Result {
  write!(f, "non-exhaustive {kind}: missing ")?;
  assert!(!pats.is_empty());
  let max_len = 3usize;
  let iter =
    pats.iter().take(max_len).map(|pat| Backticks(PatDisplay { pat, syms, prec: PatPrec::Min }));
  comma_seq(f, iter)?;
  if let Some(n) = pats.len().checked_sub(max_len) {
    if n != 0 {
      let s = if n == 1 { "" } else { "s" };
      write!(f, ", and {n} other{s}")?;
    }
  }
  Ok(())
}

fn unwrap_non_or(pat: &Pat) -> &ConPat<Lang> {
  match &pat.raw {
    RawPat::Con(c) => c,
    RawPat::Or(_) => unreachable!("witness to non-exhaustive should not be Or"),
  }
}

struct PatDisplay<'a> {
  pat: &'a Pat,
  syms: &'a Syms,
  prec: PatPrec,
}

impl fmt::Display for PatDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    ConPatDisplay { pat: unwrap_non_or(self.pat), syms: self.syms, prec: self.prec }.fmt(f)
  }
}

struct ConPatDisplay<'a> {
  pat: &'a ConPat<Lang>,
  syms: &'a Syms,
  prec: PatPrec,
}

impl fmt::Display for ConPatDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let args = self.pat.args.as_slice();
    match &self.pat.con {
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
          && labels.len() != 1
          && labels.iter().enumerate().all(|(idx, lab)| sml_hir::Lab::tuple(idx) == *lab);
        if is_tuple {
          f.write_str("(")?;
          let pats = args.iter().map(|pat| PatDisplay { pat, syms: self.syms, prec: PatPrec::Min });
          comma_seq(f, pats)?;
          f.write_str(")")?;
        } else {
          f.write_str("{")?;
          let rows = labels
            .iter()
            .zip(args)
            .map(|(lab, pat)| RowDisplay::Row { lab, pat, syms: self.syms })
            .chain(allows_other.then_some(RowDisplay::Rest));
          comma_seq(f, rows)?;
          f.write_str("}")?;
        }
      }
      Con::Variant(_, name) => {
        let name = match name {
          VariantName::Name(name) => name.as_str(),
          VariantName::Exn(exn) => self.syms.get_exn(*exn).path.last().as_str(),
        };
        let needs_paren = !args.is_empty() && matches!(self.prec, PatPrec::App);
        // these names are guaranteed not to be rebound, so they always are list constructors.
        if matches!(name, "nil" | "::") {
          let mut ac = Vec::new();
          match list_pat(&mut ac, self.pat) {
            // does not need paren because list literal patterns are atomic
            ListPatLen::Known => {
              f.write_str("[")?;
              let pats = ac.into_iter().map(|pat| ConPatDisplay {
                pat,
                syms: self.syms,
                prec: PatPrec::Min,
              });
              comma_seq(f, pats)?;
              f.write_str("]")?;
            }
            ListPatLen::Unknown => {
              if needs_paren {
                f.write_str("(")?;
              }
              let pats = ac.into_iter().map(|pat| ConPatDisplay {
                pat,
                syms: self.syms,
                prec: PatPrec::App,
              });
              sep_seq(f, " :: ", pats)?;
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
            let pats =
              args.iter().map(|pat| PatDisplay { pat, syms: self.syms, prec: PatPrec::App });
            comma_seq(f, pats)?;
          }
          if needs_paren {
            f.write_str(")")?;
          }
        }
      }
      Con::Vector(_) => {
        f.write_str("#[")?;
        let pats =
          self.pat.args.iter().map(|pat| PatDisplay { pat, syms: self.syms, prec: PatPrec::Min });
        comma_seq(f, pats)?;
        f.write_str("]")?;
      }
    }
    Ok(())
  }
}

enum ListPatLen {
  Known,
  Unknown,
}

fn list_pat<'p>(ac: &mut Vec<&'p ConPat<Lang>>, mut pat: &'p ConPat<Lang>) -> ListPatLen {
  loop {
    let name = match &pat.con {
      Con::Any => {
        ac.push(pat);
        return ListPatLen::Unknown;
      }
      Con::Variant(_, VariantName::Name(name)) => name.as_str(),
      _ => unreachable!("only Any and Variant cons can have list type"),
    };
    match name {
      "nil" => {
        assert!(pat.args.is_empty());
        return ListPatLen::Known;
      }
      "::" => {
        assert_eq!(pat.args.len(), 1, ":: has 1 argument");
        let arg_pat = match &pat.args.first().unwrap().raw {
          RawPat::Con(x) => x,
          RawPat::Or(_) => unreachable!("the argument to :: is a con pat"),
        };
        let Con::Record { allows_other: false, labels } = &arg_pat.con else {
          unreachable!("the argument to :: is a record that does not allow others")
        };
        let is_2_tup =
          labels.len() == 2 && (0..2).all(|x| labels.contains(&sml_hir::Lab::tuple(x)));
        assert!(is_2_tup, "the argument to :: is a 2-tuple");
        let (hd, tl) = match &arg_pat.args[..] {
          [a, b] => (unwrap_non_or(a), unwrap_non_or(b)),
          _ => unreachable!("the argument to :: is a 2-tuple"),
        };
        ac.push(hd);
        pat = tl;
      }
      _ => unreachable!("the list constructors are nil and ::"),
    }
  }
}

#[derive(Debug, Clone, Copy)]
enum PatPrec {
  Min,
  App,
}

enum RowDisplay<'a> {
  Row { lab: &'a sml_hir::Lab, pat: &'a Pat, syms: &'a Syms },
  Rest,
}

impl fmt::Display for RowDisplay<'_> {
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

struct Backticks<T>(T);

impl<T> fmt::Display for Backticks<T>
where
  T: fmt::Display,
{
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "`{}`", self.0)
  }
}
