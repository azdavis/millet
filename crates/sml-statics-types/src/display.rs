//! Displaying some types.

use crate::fmt_util::idx_to_name;
use crate::sym::{Sym, Syms};
use crate::ty::{
  BoundTyVars, RecordData, Ty, TyData, TyScheme, TyVarKind, Tys, UnsolvedMetaTyVarKind,
};
use crate::unify::Incompatible;
use fast_hash::FxHashMap;
use fmt_util::comma_seq;
use std::fmt;

impl Ty {
  /// Returns a value that displays this.
  #[must_use]
  pub fn display<'a>(
    self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
    lines: config::ErrorLines,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      cx: TyDisplayCx { bound_vars: None, meta_vars, syms },
      ty: self,
      prec: TyPrec::Arrow,
      pretty: Pretty::from_error_lines(lines),
    }
  }
}

impl TyScheme {
  /// Returns a value that displays this.
  #[must_use]
  pub fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
    lines: config::ErrorLines,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      cx: TyDisplayCx { bound_vars: Some(&self.bound_vars), meta_vars, syms },
      ty: self.ty,
      prec: TyPrec::Arrow,
      pretty: Pretty::from_error_lines(lines),
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
struct Pretty {
  indent: u16,
}

impl Pretty {
  const RECORD_ROW_COUNT: usize = 3;
  const RECORD_ROW_INDENT: u16 = 2;

  fn from_error_lines(lines: config::ErrorLines) -> Option<Pretty> {
    match lines {
      config::ErrorLines::One => None,
      config::ErrorLines::Many => Some(Pretty::default()),
    }
  }
}

#[derive(Clone, Copy)]
struct TyDisplayCx<'a> {
  bound_vars: Option<&'a BoundTyVars>,
  meta_vars: &'a MetaVarNames<'a>,
  syms: &'a Syms,
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
    let (ty, data) = self.cx.meta_vars.tys.canonicalize(self.ty);
    match data {
      TyData::None => f.write_str("_")?,
      TyData::BoundVar(bv) => {
        let vars = self.cx.bound_vars.expect("bound ty var without a BoundTyVars");
        let equality = matches!(bv.index_into(vars), TyVarKind::Equality);
        let name = bv.name(equality);
        write!(f, "{name}")?;
      }
      TyData::UnsolvedMetaVar(mv) => match &mv.kind {
        UnsolvedMetaTyVarKind::Kind(kind) => match kind {
          TyVarKind::Regular => {
            let &idx = self.cx.meta_vars.map.get(&ty).ok_or(fmt::Error)?;
            meta_var_idx(f, idx, "?")?;
          }
          TyVarKind::Equality => {
            let &idx = self.cx.meta_vars.map.get(&ty).ok_or(fmt::Error)?;
            meta_var_idx(f, idx, "??")?;
          }
          TyVarKind::Overloaded(ov) => ov.fmt(f)?,
        },
        UnsolvedMetaTyVarKind::UnresolvedRecord(ur) => {
          RecordMetaVarDisplay { cx: self.cx, rows: &ur.rows, pretty: self.pretty }.fmt(f)?;
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
          self.with(ty, TyPrec::App, None).fmt(f)?;
          for &ty in tys {
            f.write_str(" * ")?;
            self.with(ty, TyPrec::App, None).fmt(f)?;
          }
          if needs_parens {
            f.write_str(")")?;
          }
        } else {
          match (rows.len() >= Pretty::RECORD_ROW_COUNT, self.pretty) {
            (true, Some(pretty)) => {
              f.write_str("{")?;
              let indent = pretty.indent + Pretty::RECORD_ROW_INDENT;
              write_nl_indent(f, indent)?;
              let mut iter = rows.iter().map(|(lab, &ty)| RowDisplay {
                cx: self.cx,
                lab,
                ty,
                pretty: Some(Pretty { indent }),
              });
              let fst = iter.next().unwrap();
              fst.fmt(f)?;
              for row in iter {
                f.write_str(",")?;
                write_nl_indent(f, indent)?;
                row.fmt(f)?;
              }
              write_nl_indent(f, pretty.indent)?;
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
        let mut args_iter = data.args.iter();
        if let Some(&arg) = args_iter.next() {
          if data.args.len() == 1 {
            self.with(arg, TyPrec::App, None).fmt(f)?;
          } else {
            f.write_str("(")?;
            self.with(arg, TyPrec::Arrow, None).fmt(f)?;
            for &arg in args_iter {
              f.write_str(", ")?;
              self.with(arg, TyPrec::Arrow, None).fmt(f)?;
            }
            f.write_str(")")?;
          }
          f.write_str(" ")?;
        }
        SymDisplay { sym: data.sym, syms: self.cx.syms }.fmt(f)?;
      }
      TyData::Fn(data) => {
        let needs_parens = self.prec > TyPrec::Arrow;
        if needs_parens {
          f.write_str("(")?;
        }
        self.with(data.param, TyPrec::Star, self.pretty).fmt(f)?;
        f.write_str(" -> ")?;
        self.with(data.res, TyPrec::Arrow, self.pretty).fmt(f)?;
        if needs_parens {
          f.write_str(")")?;
        }
      }
    }
    Ok(())
  }
}

fn write_nl_indent(f: &mut fmt::Formatter<'_>, indent: u16) -> fmt::Result {
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

/// Gives names to meta variables, like `?a` or `<wordint>` or `int`.
#[derive(Debug)]
pub struct MetaVarNames<'a> {
  next_idx: usize,
  map: FxHashMap<Ty, idx::Idx>,
  tys: &'a Tys,
}

impl<'a> MetaVarNames<'a> {
  /// Returns a new one of this.
  #[must_use]
  pub fn new(tys: &'a Tys) -> Self {
    Self { next_idx: 0, map: FxHashMap::default(), tys }
  }

  /// Clears this of all the names.
  pub fn clear(&mut self) {
    self.next_idx = 0;
    self.map.clear();
  }

  /// Adds names for all the unsolved meta vars in `ty` that need names.
  pub fn extend_for(&mut self, ty: Ty) {
    self.tys.unsolved_meta_vars(ty, &mut |mv, data| match &data.kind {
      UnsolvedMetaTyVarKind::Kind(kind) => match kind {
        TyVarKind::Regular | TyVarKind::Equality => {
          self.map.entry(mv).or_insert_with(|| {
            let ret = idx::Idx::new(self.next_idx);
            self.next_idx += 1;
            ret
          });
        }
        TyVarKind::Overloaded(_) => {}
      },
      UnsolvedMetaTyVarKind::UnresolvedRecord(ur) => {
        for &ty in ur.rows.values() {
          self.extend_for(ty);
        }
      }
    });
  }
}

fn meta_var_idx(f: &mut fmt::Formatter<'_>, idx: idx::Idx, s: &str) -> fmt::Result {
  f.write_str(s)?;
  for c in idx_to_name(idx.to_usize()) {
    write!(f, "{c}")?;
  }
  Ok(())
}

/// Returns a value that displays this unresolved record meta var.
#[must_use]
pub fn record_meta_var<'a>(
  meta_vars: &'a MetaVarNames<'a>,
  syms: &'a Syms,
  rows: &'a RecordData,
  lines: config::ErrorLines,
) -> impl fmt::Display + 'a {
  RecordMetaVarDisplay {
    cx: TyDisplayCx { bound_vars: None, meta_vars, syms },
    rows,
    pretty: Pretty::from_error_lines(lines),
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
        let indent = pretty.indent + Pretty::RECORD_ROW_INDENT;
        f.write_str("{")?;
        for (lab, &ty) in self.rows {
          write_nl_indent(f, indent)?;
          RowDisplay { cx: self.cx, lab, ty, pretty: Some(Pretty { indent }) }.fmt(f)?;
          f.write_str(",")?;
        }
        write_nl_indent(f, indent)?;
        f.write_str("...")?;
        write_nl_indent(f, pretty.indent)?;
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
  pub fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
    lines: config::ErrorLines,
  ) -> impl fmt::Display + 'a {
    IncompatibleDisplay { flavor: self, meta_vars, syms, lines }
  }

  /// Extends the meta var names for all the types in this.
  ///
  /// Need to have this be separate from the Display impl so that the mutable borrow doesn't last
  /// too long.
  pub fn extend_meta_var_names(&self, meta_vars: &mut MetaVarNames<'_>) {
    match self {
      Self::FixedTyVar(_, _)
      | Self::MissingRow(_)
      | Self::Con(_, _)
      | Self::OverloadCon(_, _)
      | Self::OverloadUnify(_, _)
      | Self::UnresolvedRecordMissingRow(_) => {}
      Self::ExtraRows(record) | Self::OverloadRecord(_, record) => {
        for &ty in record.values() {
          meta_vars.extend_for(ty);
        }
      }
      Self::OverloadHeadMismatch(_, ty) | Self::NotEqTy(ty, _) => meta_vars.extend_for(*ty),
      Self::UnresolvedRecordHeadMismatch(record, ty) => {
        for &ty in record.values() {
          meta_vars.extend_for(ty);
        }
        meta_vars.extend_for(*ty);
      }
      Self::HeadMismatch(ty1, ty2) => {
        meta_vars.extend_for(*ty1);
        meta_vars.extend_for(*ty2);
      }
    }
  }
}

struct IncompatibleDisplay<'a> {
  flavor: &'a Incompatible,
  meta_vars: &'a MetaVarNames<'a>,
  syms: &'a Syms,
  lines: config::ErrorLines,
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
        let a = a.display(self.syms);
        let b = b.display(self.syms);
        write!(f, "`{a}` and `{b}` are different type constructors")
      }
      Incompatible::HeadMismatch(a, b) => {
        let a_display = a.display(self.meta_vars, self.syms, self.lines);
        let b_display = b.display(self.meta_vars, self.syms, self.lines);
        let a_desc = a.desc();
        let b_desc = b.desc();
        write!(f, "`{a_display}` is {a_desc}, but `{b_display}` is {b_desc}")
      }
      Incompatible::OverloadCon(ov, s) => {
        let s = s.display(self.syms);
        write!(f, "`{s}` is not compatible with the `{ov}` overload")
      }
      Incompatible::OverloadUnify(want, got) => {
        write!(f, "`{want}` and `{got}` are incompatible overloads")
      }
      Incompatible::OverloadRecord(ov, _) => {
        write!(f, "record types are not compatible with the `{ov}` overload")
      }
      Incompatible::OverloadHeadMismatch(ov, ty) => {
        let ty_display = ty.display(self.meta_vars, self.syms, self.lines);
        let ty_desc = ty.desc();
        write!(f, "`{ov}` is an overloaded type, but `{ty_display}` is {ty_desc}")
      }
      Incompatible::UnresolvedRecordMissingRow(lab) => {
        write!(f, "unresolved record type is missing field: `{lab}`")
      }
      Incompatible::UnresolvedRecordHeadMismatch(rows, ty) => {
        let ty_display = ty.display(self.meta_vars, self.syms, self.lines);
        let ty_desc = ty.desc();
        let rows = record_meta_var(self.meta_vars, self.syms, rows, self.lines);
        write!(f, "`{rows}` is an unresolved record type, but `{ty_display}` is {ty_desc}")
      }
      Incompatible::NotEqTy(ty, not_eq) => {
        let ty = ty.display(self.meta_vars, self.syms, self.lines);
        write!(f, "not an equality type because it contains {not_eq}: `{ty}`")
      }
    }
  }
}
