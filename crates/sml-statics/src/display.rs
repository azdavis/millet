//! Displaying some types.

use crate::types::{
  BoundTyVars, MetaTyVar, MetaVarInfo, RecordTy, Subst, Sym, Syms, Ty, TyScheme, TyVarKind,
};
use crate::{fmt_util::idx_to_name, util::meta_vars};
use fast_hash::FxHashMap;
use fmt_util::comma_seq;
use std::fmt;

impl Ty {
  pub(crate) fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      cx: TyDisplayCx { bound_vars: None, meta_vars, syms },
      ty: self,
      prec: TyPrec::Arrow,
    }
  }
}

impl TyScheme {
  pub(crate) fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      cx: TyDisplayCx { bound_vars: Some(&self.bound_vars), meta_vars, syms },
      ty: &self.ty,
      prec: TyPrec::Arrow,
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
  ty: &'a Ty,
  prec: TyPrec,
}

impl<'a> TyDisplay<'a> {
  fn with(&self, ty: &'a Ty, prec: TyPrec) -> Self {
    Self { ty, cx: self.cx, prec }
  }
}

impl fmt::Display for TyDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty {
      Ty::None => f.write_str("_")?,
      Ty::BoundVar(bv) => {
        let vars = self.cx.bound_vars.expect("bound ty var without a BoundTyVars");
        let equality = matches!(bv.index_into(vars), Some(TyVarKind::Equality));
        let name = bv.name(equality);
        write!(f, "{name}")?;
      }
      Ty::MetaVar(mv) => {
        let &idx = self.cx.meta_vars.map.get(mv).ok_or(fmt::Error)?;
        match self.cx.meta_vars.info.get(*mv) {
          Some(TyVarKind::Overloaded(ov)) => ov.fmt(f)?,
          Some(TyVarKind::Equality) => meta_var_idx(f, idx, "??")?,
          Some(TyVarKind::Record(rows, _)) => {
            RecordMetaVarDisplay { cx: self.cx, rows }.fmt(f)?;
          }
          None => meta_var_idx(f, idx, "?")?,
        }
      }
      Ty::FixedVar(fv) => fv.fmt(f)?,
      Ty::Record(rows) => {
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
          let ty = tys.next().unwrap();
          self.with(ty, TyPrec::App).fmt(f)?;
          for ty in tys {
            f.write_str(" * ")?;
            self.with(ty, TyPrec::App).fmt(f)?;
          }
          if needs_parens {
            f.write_str(")")?;
          }
        } else {
          f.write_str("{ ")?;
          comma_seq(f, rows.iter().map(|(lab, ty)| RowDisplay { cx: self.cx, lab, ty }))?;
          f.write_str(" }")?;
        }
      }
      Ty::Con(args, sym) => {
        let mut args_iter = args.iter();
        if let Some(arg) = args_iter.next() {
          if args.len() == 1 {
            self.with(arg, TyPrec::App).fmt(f)?;
          } else {
            f.write_str("(")?;
            self.with(arg, TyPrec::Arrow).fmt(f)?;
            for arg in args_iter {
              f.write_str(", ")?;
              self.with(arg, TyPrec::Arrow).fmt(f)?;
            }
            f.write_str(")")?;
          }
          f.write_str(" ")?;
        }
        SymDisplay { sym: *sym, syms: self.cx.syms }.fmt(f)?;
      }
      Ty::Fn(param, res) => {
        let needs_parens = self.prec > TyPrec::Arrow;
        if needs_parens {
          f.write_str("(")?;
        }
        self.with(param, TyPrec::Star).fmt(f)?;
        f.write_str(" -> ")?;
        self.with(res, TyPrec::Arrow).fmt(f)?;
        if needs_parens {
          f.write_str(")")?;
        }
      }
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  Arrow,
  Star,
  App,
}

struct RowDisplay<'a> {
  cx: TyDisplayCx<'a>,
  lab: &'a sml_hir::Lab,
  ty: &'a Ty,
}

impl fmt::Display for RowDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(self.lab, f)?;
    f.write_str(" : ")?;
    let td = TyDisplay { cx: self.cx, ty: self.ty, prec: TyPrec::Arrow };
    fmt::Display::fmt(&td, f)
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
pub(crate) struct MetaVarNames<'a> {
  next_idx: usize,
  map: FxHashMap<MetaTyVar, idx::Idx>,
  info: &'a MetaVarInfo,
}

impl<'a> MetaVarNames<'a> {
  pub(crate) fn new(info: &'a MetaVarInfo) -> Self {
    Self { next_idx: 0, map: FxHashMap::default(), info }
  }

  pub(crate) fn clear(&mut self) {
    self.next_idx = 0;
    self.map.clear();
  }

  pub(crate) fn extend_for(&mut self, ty: &Ty) {
    meta_vars(&Subst::default(), ty, &mut |mv, kind| {
      assert!(kind.is_none(), "the Subst was empty");
      self.map.entry(mv).or_insert_with(|| {
        let ret = idx::Idx::new(self.next_idx);
        self.next_idx += 1;
        ret
      });
      if let Some(TyVarKind::Record(rows, _)) = self.info.get(mv) {
        for ty in rows.values() {
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

struct RecordMetaVarDisplay<'a> {
  cx: TyDisplayCx<'a>,
  rows: &'a RecordTy,
}

impl fmt::Display for RecordMetaVarDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("{ ")?;
    for (lab, ty) in self.rows {
      RowDisplay { cx: self.cx, lab, ty }.fmt(f)?;
      f.write_str(", ")?;
    }
    f.write_str("... }")
  }
}
