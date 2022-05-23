//! Types.

mod sym;

use rustc_hash::FxHashMap;
use std::collections::BTreeMap;
use std::fmt;
use uniq::{Uniq, UniqGen};

pub(crate) use sym::Sym;

/// Definition: Type
#[derive(Debug, Clone)]
pub(crate) enum Ty {
  None,
  /// Can only appear when this Ty is wrapped in a TyScheme.
  BoundVar(BoundTyVar),
  /// To be substituted for a real type by the inference algorithm.
  MetaVar(MetaTyVar),
  /// Definition: RowType
  Record(BTreeMap<hir::Lab, Ty>),
  /// Definition: ConsType
  Con(Vec<Ty>, Sym),
  /// Definition: FunType
  Fn(Box<Ty>, Box<Ty>),
}

impl Ty {
  /// Returns a [`Self::Con`] with 0 arguments and the given `sym`.
  pub(crate) fn zero(sym: Sym) -> Self {
    Self::Con(Vec::new(), sym)
  }
}

/// Definition: TypeScheme, TypeFcn
#[derive(Debug)]
pub(crate) struct TyScheme {
  pub(crate) vars: TyVars,
  pub(crate) ty: Ty,
}

impl TyScheme {
  pub(crate) fn mono(ty: Ty) -> Self {
    Self {
      vars: TyVars { inner: Vec::new() },
      ty,
    }
  }

  pub(crate) fn display<'a>(&'a self, syms: &'a Syms) -> impl fmt::Display + 'a {
    TyDisplay {
      ty: &self.ty,
      vars: &self.vars,
      syms,
      prec: TyPrec::Arrow,
    }
  }
}

/// Definition: TyVar
///
/// Basically a de Bruijn index.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct BoundTyVar(usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct MetaTyVar(Uniq);

#[derive(Debug, Default)]
pub(crate) struct MetaTyVarGen(UniqGen);

impl MetaTyVarGen {
  pub(crate) fn gen(&mut self) -> MetaTyVar {
    MetaTyVar(self.0.gen())
  }
}

#[derive(Debug)]
pub(crate) struct TyVars {
  /// The length gives how many ty vars are brought into scope. The ith `bool` says whether the type
  /// variable i is equality.
  inner: Vec<bool>,
}

/// A mapping from [`Sym`]s to [`TyInfo`]s.
#[derive(Debug, Default)]
pub(crate) struct Syms {
  store: Vec<TyInfo>,
}

impl Syms {
  pub(crate) fn is_empty(&self) -> bool {
    self.store.is_empty()
  }

  pub(crate) fn insert(&mut self, ty_info: TyInfo) -> Sym {
    let ret = Sym(self.store.len());
    self.store.push(ty_info);
    ret
  }

  pub(crate) fn get(&self, sym: &Sym) -> &TyInfo {
    self.store.get(sym.0).unwrap()
  }
}

/// Definition: TyStr
#[derive(Debug)]
pub(crate) struct TyInfo {
  pub(crate) name: hir::Name,
  pub(crate) ty_scheme: TyScheme,
  pub(crate) val_env: ValEnv,
}

/// Definition: StrEnv
pub(crate) type StrEnv = FxHashMap<hir::Name, Env>;

/// Definition: TyEnv
pub(crate) type TyEnv = FxHashMap<hir::Name, Sym>;

/// Definition: ValEnv
pub(crate) type ValEnv = FxHashMap<hir::Name, ValInfo>;

#[derive(Debug)]
pub(crate) struct ValInfo {
  pub(crate) ty_scheme: TyScheme,
  pub(crate) id_status: IdStatus,
}

/// Definition: IdStatus
#[derive(Debug)]
pub(crate) enum IdStatus {
  Con,
  Exn,
  Val,
}

/// Definition: Env
pub(crate) struct Env {
  pub(crate) str_env: StrEnv,
  pub(crate) ty_env: TyEnv,
  pub(crate) val_env: ValEnv,
}

// formatting //

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TyPrec {
  Arrow,
  Star,
  App,
}

struct TyDisplay<'a> {
  ty: &'a Ty,
  vars: &'a TyVars,
  syms: &'a Syms,
  prec: TyPrec,
}

impl<'a> TyDisplay<'a> {
  fn with(&self, ty: &'a Ty, prec: TyPrec) -> Self {
    Self {
      ty,
      vars: self.vars,
      syms: self.syms,
      prec,
    }
  }
}

impl<'a> fmt::Display for TyDisplay<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.ty {
      Ty::None => f.write_str("_")?,
      Ty::BoundVar(v) => {
        f.write_str(if self.vars.inner[v.0] { "''" } else { "'" })?;
        let alpha = (b'z' - b'a') as usize;
        let quot = v.0 / alpha;
        let rem = v.0 % alpha;
        let ch = char::from((rem as u8) + b'a');
        for _ in 0..=quot {
          write!(f, "{ch}")?;
        }
      }
      // not real syntax
      Ty::MetaVar(v) => write!(f, "'{}", v.0)?,
      Ty::Record(rows) => {
        if rows.is_empty() {
          return f.write_str("unit");
        }
        let is_tuple = rows.len() > 1
          && rows
            .keys()
            .enumerate()
            .all(|(idx, lab)| hir::Lab::tuple(idx) == *lab);
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
          let mut rows = rows.iter();
          let (lab, ty) = rows.next().unwrap();
          display_row(f, self.vars, self.syms, lab, ty)?;
          for (lab, ty) in rows {
            f.write_str(", ")?;
            display_row(f, self.vars, self.syms, lab, ty)?;
          }
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
        self.syms.get(sym).name.fmt(f)?
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

fn display_row<'a>(
  f: &mut fmt::Formatter<'_>,
  vars: &'a TyVars,
  syms: &'a Syms,
  lab: &hir::Lab,
  ty: &'a Ty,
) -> fmt::Result {
  display_lab(f, lab)?;
  f.write_str(" : ")?;
  let td = TyDisplay {
    ty,
    vars,
    syms,
    prec: TyPrec::Arrow,
  };
  fmt::Display::fmt(&td, f)
}

fn display_lab(f: &mut fmt::Formatter<'_>, lab: &hir::Lab) -> fmt::Result {
  match lab {
    hir::Lab::Name(name) => fmt::Display::fmt(name, f),
    hir::Lab::Num(n) => fmt::Display::fmt(n, f),
  }
}
