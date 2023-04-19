//! The dynamic semantics, aka, running a program.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]
// TODO remove
#![allow(dead_code)]

use fast_hash::FxHashMap;
use sml_mir::{Arm, Exp, Lab, Pat, SCon};
use std::collections::BTreeMap;
use uniq::Uniq;

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
      Val::Record(rows) => {
        Exp::Record(rows.into_iter().map(|(lab, val)| (lab, Exp::from(val))).collect())
      }
      Val::Closure(_, matcher) => Exp::Fn(matcher),
    }
  }
}

type ValEnv = FxHashMap<Uniq, Val>;

enum Top {
  Exp(Exp),
  Val(Val),
  Raise(Val),
}

struct Frame {
  env: ValEnv,
  kind: FrameKind,
}

impl Frame {
  fn new(env: ValEnv, kind: FrameKind) -> Self {
    Self { env, kind }
  }
}

enum FrameKind {
  Record(std::vec::IntoIter<(Lab, Exp)>, Lab, BTreeMap<Lab, Val>),
  AppFunc(Exp),
  AppArg(Vec<Arm>),
  Raise,
  Handle(Vec<Arm>),
}

fn eval(exp: Exp) -> Result<Val, Val> {
  let mut env = ValEnv::default();
  let mut frames = Vec::<Frame>::new();
  let mut top = Top::Exp(exp);
  loop {
    top = step(&mut env, &mut frames, top);
    if frames.is_empty() {
      break;
    }
  }
  match top {
    Top::Exp(_) => unreachable!("stuck exp"),
    Top::Val(v) => Ok(v),
    Top::Raise(v) => Err(v),
  }
}

/// i think this is called a "stack machine". it is NOT recursive.
fn step(env: &mut ValEnv, frames: &mut Vec<Frame>, top: Top) -> Top {
  match top {
    Top::Exp(e) => match e {
      Exp::SCon(scon) => Top::Val(Val::SCon(scon)),
      Exp::Var(name) => Top::Val(env.get(&name).unwrap().clone()),
      Exp::Con(name) => Top::Val(Val::Con(name, None)),
      Exp::Record(exp_rows) => {
        let mut exp_rows = exp_rows.into_iter();
        match exp_rows.next() {
          None => Top::Val(Val::Record(BTreeMap::new())),
          Some((lab, exp)) => {
            frames.push(Frame::new(env.clone(), FrameKind::Record(exp_rows, lab, BTreeMap::new())));
            Top::Exp(exp)
          }
        }
      }
      Exp::Let(_, _) => todo!(),
      Exp::App(func, arg) => {
        frames.push(Frame::new(env.clone(), FrameKind::AppFunc(*arg)));
        Top::Exp(*func)
      }
      Exp::Handle(exp, matcher) => {
        frames.push(Frame::new(env.clone(), FrameKind::Handle(matcher)));
        Top::Exp(*exp)
      }
      Exp::Raise(exp) => {
        // maybe don't care about the env for raise?
        frames.push(Frame::new(ValEnv::default(), FrameKind::Raise));
        Top::Exp(*exp)
      }
      Exp::Fn(matcher) => Top::Val(Val::Closure(env.clone(), matcher)),
    },
    Top::Val(val) => match frames.pop() {
      // done evaluating
      None => Top::Val(val),
      Some(frame) => match frame.kind {
        FrameKind::Record(mut exp_rows, lab, mut val_rows) => {
          assert!(val_rows.insert(lab, val).is_none());
          match exp_rows.next() {
            None => Top::Val(Val::Record(val_rows)),
            Some((lab, exp)) => {
              *env = frame.env.clone();
              frames.push(Frame::new(frame.env, FrameKind::Record(exp_rows, lab, val_rows)));
              Top::Exp(exp)
            }
          }
        }
        FrameKind::AppFunc(arg) => match val {
          Val::Closure(clos_env, matcher) => {
            *env = frame.env;
            frames.push(Frame::new(clos_env, FrameKind::AppArg(matcher)));
            Top::Exp(arg)
          }
          _ => unreachable!("fun val not closure"),
        },
        FrameKind::AppArg(matcher) => {
          let mut ac = ValEnv::default();
          for arm in matcher {
            if pat_match(&mut ac, &arm.pat, &val) {
              *env = frame.env;
              env.extend(ac);
              return Top::Exp(arm.exp);
            }
          }
          todo!("non-exhaustive fn match")
        }
        FrameKind::Raise => Top::Raise(val),
        // handle wasn't needed, as head didn't raise
        FrameKind::Handle(_) => Top::Val(val),
      },
    },
    Top::Raise(val) => match frames.pop() {
      // unhandled exception
      None => Top::Raise(val),
      Some(frame) => match frame.kind {
        FrameKind::Handle(matcher) => {
          let mut ac = ValEnv::default();
          for arm in matcher {
            if pat_match(&mut ac, &arm.pat, &val) {
              *env = frame.env;
              env.extend(ac);
              return Top::Exp(arm.exp);
            }
          }
          // handle didn't catch the exception. keep bubbling up
          Top::Raise(val)
        }
        // for all other frames, the exception continues to bubble up
        _ => Top::Raise(val),
      },
    },
  }
}

fn pat_match(ac: &mut ValEnv, pat: &Pat, val: &Val) -> bool {
  match (pat, val) {
    (Pat::Wild, _) => true,
    (Pat::Var(name), _) => {
      assert!(ac.insert(*name, val.clone()).is_none());
      true
    }
    (_, Val::Closure(_, _)) => unreachable!("match non-(Wild or Var) with Closure"),
    (Pat::SCon(pat_sc), Val::SCon(val_sc)) => match (pat_sc, val_sc) {
      (SCon::Real(_), _) => unreachable!("Real pattern"),
      (_, SCon::Real(_)) => unreachable!("match non-(Wild or Var) with Real"),
      (SCon::Int(pat_int), SCon::Int(val_int)) => pat_int == val_int,
      (SCon::Word(pat_word), SCon::Word(val_word)) => pat_word == val_word,
      (SCon::Char(pat_char), SCon::Char(val_char)) => pat_char == val_char,
      (SCon::String(pat_str), SCon::String(val_str)) => pat_str == val_str,
      (SCon::Int(_) | SCon::Word(_) | SCon::Char(_) | SCon::String(_), _) => {
        unreachable!("SCon types do not match")
      }
    },
    (Pat::SCon(_), Val::Con(_, _) | Val::Record(_)) => {
      unreachable!("match SCon with (Con or Record")
    }
    (Pat::Con(pat_name, pat_arg), Val::Con(val_name, val_arg)) => {
      if pat_name != val_name {
        return false;
      }
      match (pat_arg, val_arg) {
        (None, None) => true,
        (Some(pat_arg), Some(val_arg)) => pat_match(ac, pat_arg.as_ref(), val_arg.as_ref()),
        (Some(_), None) => unreachable!("pat Con has arg but val does not"),
        (None, Some(_)) => unreachable!("pat Con has no arg but val does"),
      }
    }
    (Pat::Con(_, _), Val::SCon(_) | Val::Record(_)) => {
      unreachable!("match Con with (SCon or Record)")
    }
    (Pat::Record { rows: pat_rows, allows_other: _ }, Val::Record(val_rows)) => {
      pat_rows.iter().all(|(lab, pat)| pat_match(ac, pat, &val_rows[lab]))
    }
    (Pat::Record { .. }, Val::SCon(_) | Val::Con(_, _)) => {
      unreachable!("match Record with (SCon or Con")
    }
    (Pat::As(name, pat), val) => {
      assert!(ac.insert(*name, val.clone()).is_none());
      pat_match(ac, pat, val)
    }
    (Pat::Or(fst, rest), val) => {
      let mut or_ac = ValEnv::default();
      for pat in std::iter::once(fst.as_ref()).chain(rest) {
        if !pat_match(&mut or_ac, pat, val) {
          or_ac.clear();
          continue;
        }
        for (name, val) in or_ac {
          assert!(ac.insert(name, val.clone()).is_none());
        }
        return true;
      }
      false
    }
  }
}
