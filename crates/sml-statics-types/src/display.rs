//! Displaying some types.

use crate::sym::{Sym, Syms};
use crate::ty::{
  BoundTyVarData, BoundTyVars, MetaTyVarDisplayData, RecordData, Ty, TyData, TyScheme, TyVarKind,
  UnsolvedMetaTyVarKind,
};
use crate::{unify::Incompatible, St};
use fmt_util::comma_seq;
use std::fmt;

impl Ty {
  /// Returns a value that displays this.
  #[must_use]
  pub fn display(self, st: &St, lines: config::DiagnosticLines) -> impl fmt::Display + '_ {
    TyDisplay {
      cx: TyDisplayCx { bound_vars: None, st },
      ty: self,
      prec: TyPrec::Arrow,
      pretty: Pretty::from_diagnostic_lines(lines),
    }
  }
}

impl TyScheme {
  /// Returns a value that displays this.
  #[must_use]
  pub fn display<'a>(
    &'a self,
    st: &'a St,
    lines: config::DiagnosticLines,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      cx: TyDisplayCx { bound_vars: Some(&self.bound_vars), st },
      ty: self.ty,
      prec: TyPrec::Arrow,
      pretty: Pretty::from_diagnostic_lines(lines),
    }
  }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  Arrow,
  Star,
  App,
}

#[derive(Debug, Default, Clone, Copy)]
enum Pretty {
  #[default]
  Top,
  Record {
    indent: usize,
  },
}

impl Pretty {
  const RECORD_ROW_COUNT: usize = 3;
  const RECORD_ROW_INDENT: usize = 2;
  const FN_ARROW_COUNT: usize = 2;
  const FN_ARROW: &'static str = "-> ";

  fn from_diagnostic_lines(lines: config::DiagnosticLines) -> Option<Pretty> {
    match lines {
      config::DiagnosticLines::One => None,
      config::DiagnosticLines::Many => Some(Pretty::default()),
    }
  }

  fn record_indent(self) -> usize {
    match self {
      Pretty::Top => 0,
      Pretty::Record { indent } => indent,
    }
  }

  fn non_top(self) -> Self {
    Self::Record { indent: self.record_indent() }
  }
}

#[derive(Clone, Copy)]
struct TyDisplayCx<'a> {
  bound_vars: Option<&'a BoundTyVars>,
  st: &'a St,
}

struct TyDisplay<'a> {
  cx: TyDisplayCx<'a>,
  ty: Ty,
  prec: TyPrec,
  pretty: Option<Pretty>,
}

impl<'a> TyDisplay<'a> {
  fn with(&self, ty: Ty, prec: TyPrec, pretty: Option<Pretty>) -> Self {
    Self { ty, cx: self.cx, prec, pretty }
  }
}

impl fmt::Display for TyDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.cx.st.tys.data(self.ty) {
      TyData::None => f.write_str("_")?,
      TyData::BoundVar(bv) => {
        let vars = self.cx.bound_vars.expect("bound ty var without a BoundTyVars");
        match bv.index_into(vars) {
          BoundTyVarData::Kind(kind) => match kind {
            // NOTE this can clash with explicitly named ty vars, but currently they do not mix
            TyVarKind::Regular => bv.name(false).fmt(f)?,
            TyVarKind::Equality => bv.name(true).fmt(f)?,
            TyVarKind::Overloaded(ov) => ov.fmt(f)?,
          },
          BoundTyVarData::Named(name) => name.fmt(f)?,
        }
      }
      TyData::UnsolvedMetaVar(mv) => match &mv.kind {
        UnsolvedMetaTyVarKind::Kind(kind) => match kind {
          TyVarKind::Regular => mv.display.display(false).fmt(f)?,
          TyVarKind::Equality => mv.display.display(true).fmt(f)?,
          TyVarKind::Overloaded(ov) => ov.fmt(f)?,
        },
        UnsolvedMetaTyVarKind::UnresolvedRecord(ur) => {
          let pretty = self.pretty.map(Pretty::non_top);
          RecordMetaVarDisplay { cx: self.cx, rows: &ur.rows, pretty }.fmt(f)?;
        }
      },
      TyData::FixedVar(fv) => fv.ty_var.fmt(f)?,
      TyData::Record(rows) => {
        if rows.is_empty() {
          return f.write_str("unit");
        }
        let is_tuple = rows.len() > 1
          && rows.keys().enumerate().all(|(idx, lab)| sml_hir::Lab::tuple(idx) == *lab);
        if is_tuple {
          let needs_parens = self.prec > TyPrec::Star;
          if needs_parens {
            f.write_str("(")?;
          }
          let mut tys = rows.values();
          let &ty = tys.next().unwrap();
          let pretty = self.pretty.map(Pretty::non_top);
          self.with(ty, TyPrec::App, pretty).fmt(f)?;
          for &ty in tys {
            f.write_str(" * ")?;
            self.with(ty, TyPrec::App, pretty).fmt(f)?;
          }
          if needs_parens {
            f.write_str(")")?;
          }
        } else {
          match (rows.len() >= Pretty::RECORD_ROW_COUNT, self.pretty) {
            (true, Some(pretty)) => {
              f.write_str("{")?;
              let old_indent = pretty.record_indent();
              let new_indent = old_indent + Pretty::RECORD_ROW_INDENT;
              write_nl_indent(f, new_indent)?;
              let pretty = Some(Pretty::Record { indent: new_indent });
              let mut iter =
                rows.iter().map(|(lab, &ty)| RowDisplay { cx: self.cx, lab, ty, pretty });
              let fst = iter.next().unwrap();
              fst.fmt(f)?;
              for row in iter {
                f.write_str(",")?;
                write_nl_indent(f, new_indent)?;
                row.fmt(f)?;
              }
              write_nl_indent(f, old_indent)?;
              f.write_str("}")?;
            }
            (false, _) | (_, None) => {
              f.write_str("{ ")?;
              let iter =
                rows.iter().map(|(lab, &ty)| RowDisplay { cx: self.cx, lab, ty, pretty: None });
              comma_seq(f, iter)?;
              f.write_str(" }")?;
            }
          }
        }
      }
      TyData::Con(data) => {
        let pretty = self.pretty.map(Pretty::non_top);
        let mut args_iter = data.args.iter();
        if let Some(&arg) = args_iter.next() {
          if data.args.len() == 1 {
            self.with(arg, TyPrec::App, pretty).fmt(f)?;
          } else {
            f.write_str("(")?;
            self.with(arg, TyPrec::Arrow, pretty).fmt(f)?;
            for &arg in args_iter {
              f.write_str(", ")?;
              self.with(arg, TyPrec::Arrow, pretty).fmt(f)?;
            }
            f.write_str(")")?;
          }
          f.write_str(" ")?;
        }
        SymDisplay { sym: data.sym, syms: &self.cx.st.syms }.fmt(f)?;
      }
      TyData::Fn(data) => {
        let needs_parens = self.prec > TyPrec::Arrow;
        match (needs_parens, self.pretty) {
          (false, Some(pretty)) => match pretty {
            Pretty::Top => {
              let mut curried_tys = Vec::<Ty>::new();
              let mut cur = data.res;
              while let TyData::Fn(data) = self.cx.st.tys.data(cur) {
                curried_tys.push(data.param);
                cur = data.res;
              }
              // +1 for cur
              if curried_tys.len() + 1 >= Pretty::FN_ARROW_COUNT {
                let indent = Pretty::FN_ARROW.len();
                for _ in 0..indent {
                  f.write_str(" ")?;
                }
                let pretty = Some(Pretty::Record { indent });
                self.with(data.param, TyPrec::Star, pretty).fmt(f)?;
                for ty in curried_tys.into_iter().chain(std::iter::once(cur)) {
                  f.write_str("\n")?;
                  f.write_str(Pretty::FN_ARROW)?;
                  self.with(ty, TyPrec::Star, pretty).fmt(f)?;
                }
                return Ok(());
              }
            }
            Pretty::Record { .. } => {}
          },
          (true, Some(pretty)) => {
            assert!(matches!(pretty, Pretty::Record { .. }), "cannot need parens for Fn at top");
          }
          (_, None) => {}
        }
        let pretty = self.pretty.map(Pretty::non_top);
        if needs_parens {
          f.write_str("(")?;
        }
        self.with(data.param, TyPrec::Star, pretty).fmt(f)?;
        f.write_str(" -> ")?;
        self.with(data.res, TyPrec::Arrow, pretty).fmt(f)?;
        if needs_parens {
          f.write_str(")")?;
        }
      }
    }
    Ok(())
  }
}

fn write_nl_indent(f: &mut fmt::Formatter<'_>, indent: usize) -> fmt::Result {
  f.write_str("\n")?;
  for _ in 0..indent {
    f.write_str(" ")?;
  }
  Ok(())
}

struct RowDisplay<'a> {
  cx: TyDisplayCx<'a>,
  lab: &'a sml_hir::Lab,
  ty: Ty,
  pretty: Option<Pretty>,
}

impl fmt::Display for RowDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.lab.fmt(f)?;
    f.write_str(" : ")?;
    let td = TyDisplay { cx: self.cx, ty: self.ty, prec: TyPrec::Arrow, pretty: self.pretty };
    td.fmt(f)
  }
}

impl Sym {
  pub(crate) fn display(self, syms: &Syms) -> impl fmt::Display + '_ {
    SymDisplay { sym: self, syms }
  }
}

struct SymDisplay<'a> {
  sym: Sym,
  syms: &'a Syms,
}

impl fmt::Display for SymDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.syms.get(self.sym) {
      None => f.write_str("exn"),
      Some(sym_info) => sym_info.path.fmt(f),
    }
  }
}

impl MetaTyVarDisplayData {
  fn display(self, equality: bool) -> impl fmt::Display {
    MetaTyVarDisplay { data: self, equality }
  }
}

struct MetaTyVarDisplay {
  data: MetaTyVarDisplayData,
  equality: bool,
}

impl fmt::Display for MetaTyVarDisplay {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.data {
      MetaTyVarDisplayData::Unknown => f.write_str("_"),
      MetaTyVarDisplayData::Generalized(bv) => bv.name(self.equality).fmt(f),
    }
  }
}

/// Returns a value that displays this unresolved record meta var.
#[must_use]
pub fn record_meta_var<'a>(
  st: &'a St,
  rows: &'a RecordData,
  lines: config::DiagnosticLines,
) -> impl fmt::Display + 'a {
  RecordMetaVarDisplay {
    cx: TyDisplayCx { bound_vars: None, st },
    rows,
    pretty: Pretty::from_diagnostic_lines(lines),
  }
}

struct RecordMetaVarDisplay<'a> {
  cx: TyDisplayCx<'a>,
  rows: &'a RecordData,
  pretty: Option<Pretty>,
}

impl fmt::Display for RecordMetaVarDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // +1 for the `...` row
    match (self.rows.len() + 1 >= Pretty::RECORD_ROW_COUNT, self.pretty) {
      (true, Some(pretty)) => {
        let old_indent = pretty.record_indent();
        let new_indent = old_indent + Pretty::RECORD_ROW_INDENT;
        f.write_str("{")?;
        let pretty = Some(Pretty::Record { indent: new_indent });
        for (lab, &ty) in self.rows {
          write_nl_indent(f, new_indent)?;
          RowDisplay { cx: self.cx, lab, ty, pretty }.fmt(f)?;
          f.write_str(",")?;
        }
        write_nl_indent(f, new_indent)?;
        f.write_str("...")?;
        write_nl_indent(f, old_indent)?;
        f.write_str("}")
      }
      (false, _) | (_, None) => {
        f.write_str("{ ")?;
        for (lab, &ty) in self.rows {
          RowDisplay { cx: self.cx, lab, ty, pretty: None }.fmt(f)?;
          f.write_str(", ")?;
        }
        f.write_str("... }")
      }
    }
  }
}

impl Incompatible {
  /// Returns a value that displays this.
  #[must_use]
  pub fn display<'a>(&'a self, st: &'a St) -> impl fmt::Display + 'a {
    IncompatibleDisplay { flavor: self, st }
  }
}

struct IncompatibleDisplay<'a> {
  flavor: &'a Incompatible,
  st: &'a St,
}

impl fmt::Display for IncompatibleDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.flavor {
      Incompatible::FixedTyVar(a, b) => {
        write!(f, "`{a}` and `{b}` are different type variables")
      }
      Incompatible::MissingRow(lab) => {
        write!(f, "record type is missing field: `{lab}`")
      }
      Incompatible::ExtraRows(rows) => {
        write!(f, "record type has extra fields: ")?;
        fmt_util::comma_seq(f, rows.iter().map(|(lab, _)| lab))
      }
      Incompatible::Con(a, b) => {
        let a = a.display(&self.st.syms);
        let b = b.display(&self.st.syms);
        write!(f, "`{a}` and `{b}` are different type constructors")
      }
      Incompatible::HeadMismatch(a, b) => {
        let a_display = a.display(self.st, config::DiagnosticLines::One);
        let b_display = b.display(self.st, config::DiagnosticLines::One);
        let a_desc = a.desc();
        let b_desc = b.desc();
        write!(f, "`{a_display}` is {a_desc}, but `{b_display}` is {b_desc}")
      }
      Incompatible::OverloadCon(ov, s) => {
        let s = s.display(&self.st.syms);
        write!(f, "`{s}` is not compatible with the `{ov}` overload")
      }
      Incompatible::OverloadUnify(want, got) => {
        write!(f, "`{want}` and `{got}` are incompatible overloads")
      }
      Incompatible::OverloadRecord(ov, _) => {
        write!(f, "record types are not compatible with the `{ov}` overload")
      }
      Incompatible::OverloadHeadMismatch(ov, ty) => {
        let ty_display = ty.display(self.st, config::DiagnosticLines::One);
        let ty_desc = ty.desc();
        write!(f, "`{ov}` is an overloaded type, but `{ty_display}` is {ty_desc}")
      }
      Incompatible::UnresolvedRecordMissingRow(lab) => {
        write!(f, "unresolved record type is missing field: `{lab}`")
      }
      Incompatible::UnresolvedRecordHeadMismatch(rows, ty) => {
        let ty_display = ty.display(self.st, config::DiagnosticLines::One);
        let ty_desc = ty.desc();
        let rows = record_meta_var(self.st, rows, config::DiagnosticLines::One);
        write!(f, "`{rows}` is an unresolved record type, but `{ty_display}` is {ty_desc}")
      }
      Incompatible::NotEqTy(ty, not_eq) => {
        let ty = ty.display(self.st, config::DiagnosticLines::One);
        write!(f, "not an equality type because it contains {not_eq}: `{ty}`")
      }
    }
  }
}
