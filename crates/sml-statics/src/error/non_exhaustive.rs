use crate::pat_match::{Con, Pat, VariantName};
use crate::types::Syms;
use fmt_util::{comma_seq, sep_seq};
use pattern_match::RawPat;
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

impl fmt::Display for PatDisplay<'_> {
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
