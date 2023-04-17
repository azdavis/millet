//! The dynamic semantics, aka, running a program.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]
// TODO remove
#![allow(dead_code)]

use sml_path::Path;
use std::collections::BTreeMap;
use str_util::Name;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
struct Arm {
  pat: Pat,
  exp: Exp,
}

#[derive(Debug, Clone)]
enum Dec {
  Val(Vec<ValBind>),
  Datatype(Vec<DatBind>),
  DatatypeCopy(Name, Path),
  Exception(Vec<ExBind>),
  Local(Vec<Dec>, Vec<Dec>),
  Open(Vec<Path>),
}

#[derive(Debug, Clone)]
struct ValBind {
  rec: bool,
  pat: Pat,
  exp: Exp,
}

#[derive(Debug, Clone)]
struct DatBind {
  ty_vars: usize,
  name: Name,
  cons: Vec<ConBind>,
}

#[derive(Debug, Clone)]
struct ConBind {
  name: Name,
  ty: bool,
}

#[derive(Debug, Clone)]
enum ExBind {
  /// The bool is whether this has an `of ty`.
  New(Name, bool),
  Copy(Name, Path),
}

#[derive(Debug, Clone)]
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

/// An environment.
#[derive(Debug, Clone)]
struct Env {
  val: ValEnv,
  str: StrEnv,
}

/// Uses [`BTreeMap`] for stable order.
type ValEnv = BTreeMap<Name, Exp>;

/// Uses [`BTreeMap`] for stable order.
type StrEnv = BTreeMap<Name, Env>;

impl Env {
  fn get<'e, 'p>(&'e self, path: &'p Path) -> Result<&'e Env, &'p Name> {
    let mut ret = self;
    for name in path.prefix() {
      ret = match ret.str.get(name) {
        Some(x) => x,
        None => return Err(name),
      };
    }
    Ok(ret)
  }
}

enum Eval {
  Step(Exp),
  Val(Exp),
}

struct Raise(Exp);

fn step_exp(env: &Env, exp: Exp) -> Result<Eval, Raise> {
  match exp {
    Exp::SCon(scon) => Ok(Eval::Val(Exp::SCon(scon))),
    Exp::Path(path) => {
      let env = env.get(&path).unwrap();
      let val = env.val.get(path.last()).unwrap().clone();
      Ok(Eval::Val(val))
    }
    Exp::Record(rows) => {
      let mut new_rows = BTreeMap::<sml_hir::Lab, Exp>::new();
      let mut iter = rows.into_iter();
      for (lab, exp) in iter.by_ref() {
        match step_exp(env, exp)? {
          Eval::Step(exp) => {
            new_rows.insert(lab, exp);
            new_rows.extend(iter);
            return Ok(Eval::Step(Exp::Record(new_rows)));
          }
          Eval::Val(exp) => {
            new_rows.insert(lab, exp);
          }
        }
      }
      Ok(Eval::Val(Exp::Record(new_rows)))
    }
    Exp::Let(_, _) => todo!(),
    Exp::App(func, arg) => match step_exp(env, *func)? {
      Eval::Step(func) => Ok(Eval::Step(Exp::App(Box::new(func), arg))),
      Eval::Val(func) => match step_exp(env, *arg)? {
        Eval::Step(arg) => Ok(Eval::Step(Exp::App(Box::new(func), Box::new(arg)))),
        Eval::Val(arg) => match func {
          Exp::Fn(matcher) => {
            for arm in matcher {
              let mut ac = ValEnv::default();
              if pat_match(env, &mut ac, arm.pat, &arg) {
                // TODO update the env!
                return Ok(Eval::Step(arm.exp));
              }
            }
            Err(Raise(Exp::Path(Path::one(Name::new("Match")))))
          }
          _ => unreachable!("app func not Fn"),
        },
      },
    },
    Exp::Handle(head, matcher) => match step_exp(env, *head) {
      Ok(x) => Ok(x),
      Err(r) => {
        for arm in matcher {
          let mut ac = ValEnv::default();
          if pat_match(env, &mut ac, arm.pat, &r.0) {
            // TODO update the env!
            return Ok(Eval::Step(arm.exp));
          }
        }
        Err(r)
      }
    },
    Exp::Raise(exp) => match step_exp(env, *exp)? {
      Eval::Step(exp) => Ok(Eval::Step(Exp::Raise(Box::new(exp)))),
      Eval::Val(exp) => Err(Raise(exp)),
    },
    // TODO **closures** are values (store the env)
    Exp::Fn(matcher) => Ok(Eval::Val(Exp::Fn(matcher))),
  }
}

fn pat_match(_: &Env, _: &mut ValEnv, pat: Pat, exp: &Exp) -> bool {
  match (pat, exp) {
    (Pat::Wild, _) => true,
    _ => todo!(),
  }
}
