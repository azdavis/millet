//! The dynamic semantics, aka, running a program.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]
// TODO remove
#![allow(dead_code)]

use sml_path::Path;
use std::collections::BTreeMap;
use str_util::Name;

#[derive(Debug)]
enum Exp {
  SCon(sml_hir::SCon),
  Path(Path),
  Record(BTreeMap<sml_hir::Lab, Exp>),
  Let(Vec<Dec>, Box<Exp>),
  App(Box<Exp>, Box<Exp>),
  Handle(Box<Exp>, Vec<Arm>),
  Raise(Box<Exp>),
  Fn(Vec<Arm>),
}

#[derive(Debug)]
struct Arm {
  pat: Pat,
  exp: Exp,
}

#[derive(Debug)]
enum Dec {
  Val(Vec<ValBind>),
  Datatype(Vec<DatBind>),
  DatatypeCopy(Name, Path),
  Exception(Vec<ExBind>),
  Local(Vec<Dec>, Vec<Dec>),
  Open(Vec<Path>),
}

#[derive(Debug)]
struct ValBind {
  rec: bool,
  pat: Pat,
  exp: Exp,
}

#[derive(Debug)]
struct DatBind {
  ty_vars: usize,
  name: Name,
  cons: Vec<ConBind>,
}

#[derive(Debug)]
struct ConBind {
  name: Name,
  ty: bool,
}

#[derive(Debug)]
enum ExBind {
  /// The bool is whether this has an `of ty`.
  New(Name, bool),
  Copy(Name, Path),
}

#[derive(Debug)]
enum Pat {
  Wild,
  SCon(sml_hir::SCon),
  Con(Path, Option<Box<Pat>>),
  Record { rows: BTreeMap<sml_hir::Lab, Pat>, allows_other: bool },
  As(Name, Box<Pat>),
  Or(Box<Pat>, Vec<Pat>),
}

fn get_exp(ars: &sml_hir::Arenas, exp: sml_hir::ExpIdx) -> Option<Exp> {
  match &ars.exp[exp?] {
    sml_hir::Exp::Hole => None,
    sml_hir::Exp::SCon(scon) => Some(Exp::SCon(scon.clone())),
    sml_hir::Exp::Path(path) => Some(Exp::Path(path.clone())),
    sml_hir::Exp::Record(rows) => {
      let rows = rows
        .iter()
        .map(|(lab, exp)| Some((lab.clone(), get_exp(ars, *exp)?)))
        .collect::<Option<BTreeMap<_, _>>>()?;
      Some(Exp::Record(rows))
    }
    sml_hir::Exp::Let(decs, exp) => {
      let decs: Vec<_> = decs.iter().filter_map(|&d| get_dec(ars, d)).collect();
      let exp = get_exp(ars, *exp)?;
      Some(Exp::Let(decs, Box::new(exp)))
    }
    sml_hir::Exp::App(func, argument) => {
      let func = get_exp(ars, *func)?;
      let argument = get_exp(ars, *argument)?;
      Some(Exp::App(Box::new(func), Box::new(argument)))
    }
    sml_hir::Exp::Handle(exp, matcher) => {
      let exp = get_exp(ars, *exp)?;
      let matcher = get_matcher(ars, matcher)?;
      Some(Exp::Handle(Box::new(exp), matcher))
    }
    sml_hir::Exp::Raise(exp) => Some(Exp::Raise(Box::new(get_exp(ars, *exp)?))),
    sml_hir::Exp::Fn(matcher, _) => Some(Exp::Fn(get_matcher(ars, matcher)?)),
    sml_hir::Exp::Typed(exp, _) => get_exp(ars, *exp),
  }
}

fn get_matcher(ars: &sml_hir::Arenas, matcher: &[sml_hir::Arm]) -> Option<Vec<Arm>> {
  matcher
    .iter()
    .map(|arm| {
      let pat = get_pat(ars, arm.pat)?;
      let exp = get_exp(ars, arm.exp)?;
      Some(Arm { pat, exp })
    })
    .collect()
}

fn get_dec(ars: &sml_hir::Arenas, dec: sml_hir::DecIdx) -> Option<Dec> {
  match &ars.dec[dec] {
    sml_hir::Dec::Ty(_) | sml_hir::Dec::Abstype(_, _, _) => None,
    sml_hir::Dec::Val(_, val_binds) => {
      let val_binds = val_binds
        .iter()
        .map(|vb| {
          let pat = get_pat(ars, vb.pat)?;
          let exp = get_exp(ars, vb.exp)?;
          Some(ValBind { rec: vb.rec, pat, exp })
        })
        .collect::<Option<Vec<_>>>()?;
      Some(Dec::Val(val_binds))
    }
    sml_hir::Dec::Datatype(dat_binds, _) => {
      let dat_binds = dat_binds
        .iter()
        .map(|db| {
          let cons = db
            .cons
            .iter()
            .map(|cb| ConBind { name: cb.name.clone(), ty: cb.ty.is_some() })
            .collect();
          Some(DatBind { ty_vars: db.ty_vars.len(), name: db.name.clone(), cons })
        })
        .collect::<Option<Vec<_>>>()?;
      Some(Dec::Datatype(dat_binds))
    }
    sml_hir::Dec::DatatypeCopy(name, path) => Some(Dec::DatatypeCopy(name.clone(), path.clone())),
    sml_hir::Dec::Exception(ex_binds) => {
      let ex_binds = ex_binds
        .iter()
        .map(|eb| match eb {
          sml_hir::ExBind::New(name, ty) => ExBind::New(name.clone(), ty.is_some()),
          sml_hir::ExBind::Copy(name, path) => ExBind::Copy(name.clone(), path.clone()),
        })
        .collect();
      Some(Dec::Exception(ex_binds))
    }
    sml_hir::Dec::Local(local_decs, in_decs) => {
      let local_decs: Vec<_> = local_decs.iter().filter_map(|&dec| get_dec(ars, dec)).collect();
      let in_decs: Vec<_> = in_decs.iter().filter_map(|&dec| get_dec(ars, dec)).collect();
      Some(Dec::Local(local_decs, in_decs))
    }
    sml_hir::Dec::Open(paths) => Some(Dec::Open(paths.clone())),
  }
}

fn get_pat(ars: &sml_hir::Arenas, pat: sml_hir::PatIdx) -> Option<Pat> {
  match &ars.pat[pat?] {
    sml_hir::Pat::Wild => Some(Pat::Wild),
    sml_hir::Pat::SCon(scon) => Some(Pat::SCon(scon.clone())),
    sml_hir::Pat::Con(path, pat) => {
      let pat = match *pat {
        None => None,
        Some(pat) => Some(Box::new(get_pat(ars, pat)?)),
      };
      Some(Pat::Con(path.clone(), pat))
    }
    sml_hir::Pat::Record { rows, allows_other } => {
      let rows = rows
        .iter()
        .map(|(lab, pat)| Some((lab.clone(), get_pat(ars, *pat)?)))
        .collect::<Option<BTreeMap<_, _>>>()?;
      Some(Pat::Record { rows, allows_other: *allows_other })
    }
    sml_hir::Pat::Typed(pat, _) => get_pat(ars, *pat),
    sml_hir::Pat::As(name, pat) => {
      let pat = get_pat(ars, *pat)?;
      Some(Pat::As(name.clone(), Box::new(pat)))
    }
    sml_hir::Pat::Or(or) => {
      let first = get_pat(ars, or.first)?;
      let rest = or.rest.iter().map(|&pat| get_pat(ars, pat)).collect::<Option<Vec<_>>>()?;
      Some(Pat::Or(Box::new(first), rest))
    }
  }
}
