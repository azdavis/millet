//! The dynamic semantics, aka, running a program.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]
// TODO remove
#![allow(dead_code)]

use fast_hash::FxHashMap;
use sml_hir::{Lab, SCon};
use std::collections::BTreeMap;
use uniq::Uniq;

#[derive(Debug, Clone)]
enum Exp {
  SCon(SCon),
  Var(Uniq),
  Con(Uniq),
  Record(BTreeMap<Lab, Exp>),
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
  DatatypeCopy(Uniq, Uniq),
  Exception(Vec<ExBind>),
  Local(Vec<Dec>, Vec<Dec>),
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
  name: Uniq,
  cons: Vec<ConBind>,
}

#[derive(Debug, Clone)]
struct ConBind {
  name: Uniq,
  ty: bool,
}

#[derive(Debug, Clone)]
enum ExBind {
  /// The bool is whether this has an `of ty`.
  New(Uniq, bool),
  Copy(Uniq, Uniq),
}

#[derive(Debug, Clone)]
enum Pat {
  Wild,
  Var(Uniq),
  SCon(SCon),
  Con(Uniq, Option<Box<Pat>>),
  Record { rows: BTreeMap<Lab, Pat>, allows_other: bool },
  As(Uniq, Box<Pat>),
  Or(Box<Pat>, Vec<Pat>),
}

#[derive(Debug, Clone)]
enum Val {
  SCon(SCon),
  Con(Uniq, Option<Box<Val>>),
  Record(BTreeMap<Lab, Val>),
  Closure(ValEnv, Vec<Arm>),
}

impl From<Val> for Exp {
  fn from(val: Val) -> Self {
    match val {
      Val::SCon(scon) => Exp::SCon(scon),
      Val::Con(name, arg) => match arg {
        Some(val) => Exp::App(Box::new(Exp::Con(name)), Box::new(Exp::from(*val))),
        None => Exp::Con(name),
      },
      Val::Record(rows) => Exp::Record(exp_rows_from_val_rows(rows)),
      Val::Closure(_, matcher) => Exp::Fn(matcher),
    }
  }
}

fn exp_rows_from_val_rows(rows: BTreeMap<Lab, Val>) -> BTreeMap<Lab, Exp> {
  rows.into_iter().map(|(lab, val)| (lab, Exp::from(val))).collect()
}

type ValEnv = FxHashMap<Uniq, Val>;

enum Eval {
  Step(Exp),
  Val(Val),
}

struct Raise(Val);

fn step_exp(env: &ValEnv, exp: Exp) -> Result<Eval, Raise> {
  match exp {
    Exp::SCon(scon) => Ok(Eval::Val(Val::SCon(scon))),
    Exp::Var(name) => Ok(Eval::Val(env.get(&name).unwrap().clone())),
    Exp::Con(name) => Ok(Eval::Val(Val::Con(name, None))),
    Exp::Record(rows) => {
      let mut new_rows = BTreeMap::<Lab, Val>::new();
      let mut iter = rows.into_iter();
      for (lab, exp) in iter.by_ref() {
        match step_exp(env, exp)? {
          Eval::Step(exp) => {
            let mut new_rows = exp_rows_from_val_rows(new_rows);
            new_rows.insert(lab, exp);
            new_rows.extend(iter);
            return Ok(Eval::Step(Exp::Record(new_rows)));
          }
          Eval::Val(val) => {
            new_rows.insert(lab, val);
          }
        }
      }
      Ok(Eval::Val(Val::Record(new_rows)))
    }
    Exp::Let(_, _) => todo!("let"),
    Exp::App(func, arg) => match step_exp(env, *func)? {
      Eval::Step(func) => Ok(Eval::Step(Exp::App(Box::new(func), arg))),
      Eval::Val(func) => match step_exp(env, *arg)? {
        Eval::Step(arg) => Ok(Eval::Step(Exp::App(Box::new(func.into()), Box::new(arg)))),
        Eval::Val(arg) => match func {
          // TODO use the closure env!
          Val::Closure(_, matcher) => {
            for arm in matcher {
              let mut ac = ValEnv::default();
              if pat_match(&mut ac, arm.pat, &arg) {
                // TODO update the env!
                return Ok(Eval::Step(arm.exp));
              }
            }
            // TODO generate a con for Match to return here
            todo!("non-exhaustive match")
          }
          Val::Con(name, con_arg) => match con_arg {
            None => Ok(Eval::Val(Val::Con(name, Some(Box::new(arg))))),
            Some(_) => unreachable!("app func Con with arg"),
          },
          _ => unreachable!("app func not Fn or Con"),
        },
      },
    },
    Exp::Handle(head, matcher) => match step_exp(env, *head) {
      Ok(x) => Ok(x),
      Err(r) => {
        for arm in matcher {
          let mut ac = ValEnv::default();
          if pat_match(&mut ac, arm.pat, &r.0) {
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
    Exp::Fn(matcher) => Ok(Eval::Val(Val::Closure(env.clone(), matcher))),
  }
}

fn pat_match(ac: &mut ValEnv, pat: Pat, val: &Val) -> bool {
  match (pat, val) {
    (Pat::Wild, _) => true,
    (Pat::Var(name), _) => {
      assert!(ac.insert(name, val.clone()).is_none());
      true
    }
    (_, Val::Closure(_, _)) => unreachable!("match non-(Wild or Var) with Closure"),
    (Pat::SCon(pat_sc), Val::SCon(val_sc)) => match (pat_sc, val_sc) {
      (SCon::Int(pat_int), SCon::Int(val_int)) => pat_int == *val_int,
      (SCon::Word(pat_word), SCon::Word(val_word)) => pat_word == *val_word,
      (SCon::Char(pat_char), SCon::Char(val_char)) => pat_char == *val_char,
      (SCon::String(pat_str), SCon::String(val_str)) => pat_str == *val_str,
      (SCon::Real(_), _) | (_, SCon::Real(_)) => unreachable!("Real pattern"),
      (SCon::Int(_) | SCon::Word(_) | SCon::Char(_) | SCon::String(_), _) => {
        unreachable!("SCon types do not match")
      }
    },
    (Pat::SCon(_), Val::Con(_, _) | Val::Record(_)) => {
      unreachable!("match SCon with (Con or Record")
    }
    (Pat::Con(pat_name, pat_arg), Val::Con(val_name, val_arg)) => {
      unreachable!("match Con {pat_name:?} {pat_arg:?} {val_name:?} {val_arg:?}")
    }
    (Pat::Con(_, _), Val::SCon(_) | Val::Record(_)) => {
      unreachable!("match Con with (SCon or Record)")
    }
    (Pat::Record { rows: pat_rows, allows_other: _ }, Val::Record(val_rows)) => {
      todo!("match Record {pat_rows:?} {val_rows:?}")
    }
    (Pat::Record { .. }, Val::SCon(_) | Val::Con(_, _)) => {
      unreachable!("match Record with (SCon or Con")
    }
    (Pat::As(_, _), _) => {
      todo!("match As")
    }
    (Pat::Or(_, _), _) => {
      todo!("match Or")
    }
  }
}
