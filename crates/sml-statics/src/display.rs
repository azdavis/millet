//! Displaying some types.

use crate::fmt_util::idx_to_name;
use crate::types::{
  meta_vars, BoundTyVars, MetaTyVar, MetaVarInfo, Overload, Subst, Sym, Syms, Ty, TyScheme,
  TyVarKind,
};
use fast_hash::FxHashMap;
use fmt_util::comma_seq;
use std::fmt;

impl Ty {
  pub(crate) fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
  ) -> impl fmt::Display + 'a {
    TyDisplay { ty: self, bound_vars: None, meta_vars, syms, prec: TyPrec::Arrow }
  }
}

impl TyScheme {
  pub(crate) fn display<'a>(
    &'a self,
    meta_vars: &'a MetaVarNames<'a>,
    syms: &'a Syms,
  ) -> impl fmt::Display + 'a {
    TyDisplay {
      ty: &self.ty,
      bound_vars: Some(&self.bound_vars),
      meta_vars,
      syms,
      prec: TyPrec::Arrow,
    }
  }
}

struct TyDisplay<'a> {
  ty: &'a Ty,
  bound_vars: Option<&'a BoundTyVars>,
  meta_vars: &'a MetaVarNames<'a>,
  syms: &'a Syms,
  prec: TyPrec,
}

impl<'a> TyDisplay<'a> {
  fn with(&self, ty: &'a Ty, prec: TyPrec) -> Self {
    Self { ty, bound_vars: self.bound_vars, meta_vars: self.meta_vars, syms: self.syms, prec }
  }
}

impl fmt::Display for TyDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty {
      Ty::None => f.write_str("_")?,
      Ty::BoundVar(bv) => {
        let vars = self.bound_vars.expect("bound ty var without a BoundTyVars");
        let equality = matches!(bv.index_into(vars), Some(TyVarKind::Equality));
        let name = bv.name(equality);
        write!(f, "{name}")?;
      }
      Ty::MetaVar(mv) => {
        let name = self.meta_vars.get(*mv).ok_or(fmt::Error)?;
        write!(f, "{name}")?;
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
          comma_seq(
            f,
            rows.iter().map(|(lab, ty)| RowDisplay {
              bound_vars: self.bound_vars,
              meta_vars: self.meta_vars,
              syms: self.syms,
              lab,
              ty,
            }),
          )?;
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
        SymDisplay { sym: *sym, syms: self.syms }.fmt(f)?;
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
  bound_vars: Option<&'a BoundTyVars>,
  meta_vars: &'a MetaVarNames<'a>,
  syms: &'a Syms,
  lab: &'a sml_hir::Lab,
  ty: &'a Ty,
}

impl fmt::Display for RowDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    fmt::Display::fmt(self.lab, f)?;
    f.write_str(" : ")?;
    let td = TyDisplay {
      ty: self.ty,
      bound_vars: self.bound_vars,
      meta_vars: self.meta_vars,
      syms: self.syms,
      prec: TyPrec::Arrow,
    };
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
      Some((name, _)) => name.fmt(f),
    }
  }
}

#[derive(Debug)]
pub(crate) struct MetaVarNames<'a> {
  next_idx: usize,
  map: FxHashMap<MetaTyVar, MetaVarName>,
  info: &'a MetaVarInfo,
}

impl<'a> MetaVarNames<'a> {
  pub(crate) fn new(info: &'a MetaVarInfo) -> Self {
    Self { next_idx: 0, map: FxHashMap::default(), info }
  }

  pub(crate) fn extend_for(&mut self, ty: &Ty) {
    meta_vars(
      &Subst::default(),
      &mut |x, _| {
        self.map.entry(x).or_insert_with(|| {
          let ret = MetaVarName::Idx(idx::Idx::new(self.next_idx));
          self.next_idx += 1;
          ret
        });
      },
      ty,
    );
  }

  /// tries the [`MetaVarInfo`] first, then fall back to a generated name like `?a`, `?b`, etc.
  fn get(&self, mv: MetaTyVar) -> Option<MetaVarName> {
    self
      .info
      .get(mv)
      .and_then(|kind| match kind {
        TyVarKind::Overloaded(ov) => Some(MetaVarName::Overload(*ov)),
        TyVarKind::Equality | TyVarKind::Record(_) => None,
      })
      .or_else(|| self.map.get(&mv).copied())
  }
}

#[derive(Debug, Clone, Copy)]
enum MetaVarName {
  Idx(idx::Idx),
  Overload(Overload),
}

impl fmt::Display for MetaVarName {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match *self {
      MetaVarName::Idx(idx) => {
        f.write_str("?")?;
        for c in idx_to_name(idx.to_usize()) {
          write!(f, "{c}")?;
        }
        Ok(())
      }
      MetaVarName::Overload(ov) => ov.fmt(f),
    }
  }
}
