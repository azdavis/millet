//! The dynamic semantics, aka, running a program.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]
// TODO remove
#![allow(dead_code)]

use fast_hash::FxHashMap;
use sml_hir::{la_arena, Lab, SCon};
use sml_statics_types::info::{IdStatus, IdStatusMap};
use sml_statics_types::sym::Exn;
use std::collections::BTreeMap;
use str_util::Name;

#[derive(Debug, Clone)]
enum Val {
  SCon(SCon),
  Con(Con),
  Record(BTreeMap<Lab, Val>),
  Closure(Env, Vec<sml_hir::Arm>),
}

#[derive(Debug, Clone)]
enum ConKind {
  Dat(Name),
  Exn(Name, Exn),
}

#[derive(Debug, Clone)]
struct Con {
  kind: ConKind,
  arg: Option<Box<Val>>,
}

impl Con {
  fn empty(kind: ConKind) -> Self {
    Self { kind, arg: None }
  }
}

#[derive(Debug, Default, Clone)]
struct Env {
  str: StrEnv,
  val: ValEnv,
}

impl Env {
  fn get<'e, 'n, N>(&'e self, names: N) -> Result<&'e Env, &'n Name>
  where
    N: Iterator<Item = &'n Name>,
  {
    let mut ret = self;
    for name in names {
      ret = match ret.str.get(name) {
        Some(x) => x,
        None => return Err(name),
      };
    }
    Ok(ret)
  }
}

type StrEnv = FxHashMap<Name, Env>;
type ValEnv = FxHashMap<Name, Val>;

#[derive(Debug)]
enum Step {
  Exp(la_arena::Idx<sml_hir::Exp>),
  Val(Val),
  Raise(Val),
}

impl Step {
  fn exp(idx: sml_hir::ExpIdx) -> Self {
    Self::Exp(idx.expect("no exp"))
  }
}

#[derive(Debug)]
struct Frame {
  env: Env,
  kind: FrameKind,
}

impl Frame {
  fn new(env: Env, kind: FrameKind) -> Self {
    Self { env, kind }
  }
}

#[derive(Debug)]
enum FrameKind {
  Record(std::vec::IntoIter<(Lab, sml_hir::ExpIdx)>, Lab, BTreeMap<Lab, Val>),
  AppFunc(sml_hir::ExpIdx),
  AppArg(Vec<sml_hir::Arm>),
  Raise,
  Handle(Vec<sml_hir::Arm>),
}

#[derive(Debug, Clone, Copy)]
struct Cx<'a> {
  ars: &'a sml_hir::Arenas,
  exp: &'a IdStatusMap<sml_hir::Exp>,
  pat: &'a IdStatusMap<sml_hir::Pat>,
}

fn eval(cx: Cx<'_>, exp: sml_hir::ExpIdx) -> Result<Val, Val> {
  let mut st = St::default();
  let mut s = Step::exp(exp);
  loop {
    s = step(&mut st, cx, s);
    if st.frames.is_empty() {
      break;
    }
  }
  match s {
    Step::Exp(_) => unreachable!("stuck exp"),
    Step::Val(v) => Ok(v),
    Step::Raise(v) => Err(v),
  }
}

#[derive(Debug, Default)]
struct St {
  env: Env,
  frames: Vec<Frame>,
}

impl St {
  fn push_with_cur_env(&mut self, kind: FrameKind) {
    let env = self.env.clone();
    self.frames.push(Frame::new(env, kind));
  }
}

/// i think this is called a "stack machine". it is NOT recursive.
#[allow(clippy::too_many_lines)]
fn step(st: &mut St, cx: Cx<'_>, s: Step) -> Step {
  match s {
    Step::Exp(exp) => match &cx.ars.exp[exp] {
      sml_hir::Exp::Hole => unreachable!("exp hole"),
      sml_hir::Exp::SCon(scon) => Step::Val(Val::SCon(scon.clone())),
      sml_hir::Exp::Path(path) => match cx.exp[exp] {
        IdStatus::Con => Step::Val(Val::Con(Con::empty(ConKind::Dat(path.last().clone())))),
        IdStatus::Exn(except) => {
          Step::Val(Val::Con(Con::empty(ConKind::Exn(path.last().clone(), except))))
        }
        IdStatus::Val => {
          let env = st.env.get(path.prefix().iter()).expect("no prefix");
          Step::Val(env.val[path.last()].clone())
        }
      },
      sml_hir::Exp::Record(exp_rows) => {
        let mut exp_rows = exp_rows.clone().into_iter();
        match exp_rows.next() {
          None => Step::Val(Val::Record(BTreeMap::new())),
          Some((lab, exp)) => {
            st.push_with_cur_env(FrameKind::Record(exp_rows, lab, BTreeMap::new()));
            Step::exp(exp)
          }
        }
      }
      sml_hir::Exp::Let(_, _) => todo!(),
      sml_hir::Exp::App(func, arg) => {
        st.push_with_cur_env(FrameKind::AppFunc(*arg));
        Step::exp(*func)
      }
      sml_hir::Exp::Handle(exp, matcher) => {
        st.push_with_cur_env(FrameKind::Handle(matcher.clone()));
        Step::exp(*exp)
      }
      sml_hir::Exp::Raise(exp) => {
        // maybe don't care about the env for raise?
        st.frames.push(Frame::new(Env::default(), FrameKind::Raise));
        Step::exp(*exp)
      }
      sml_hir::Exp::Fn(matcher, _) => Step::Val(Val::Closure(st.env.clone(), matcher.clone())),
      sml_hir::Exp::Typed(exp, _) => Step::exp(*exp),
    },
    Step::Val(val) => match st.frames.pop() {
      // done evaluating
      None => Step::Val(val),
      Some(frame) => match frame.kind {
        FrameKind::Record(mut exp_rows, lab, mut val_rows) => {
          assert!(val_rows.insert(lab, val).is_none());
          match exp_rows.next() {
            None => Step::Val(Val::Record(val_rows)),
            Some((lab, exp)) => {
              st.env = frame.env;
              st.push_with_cur_env(FrameKind::Record(exp_rows, lab, val_rows));
              Step::exp(exp)
            }
          }
        }
        FrameKind::AppFunc(arg) => match val {
          Val::Closure(clos_env, matcher) => {
            st.env = frame.env;
            st.frames.push(Frame::new(clos_env, FrameKind::AppArg(matcher)));
            Step::exp(arg)
          }
          _ => unreachable!("fun val not closure"),
        },
        FrameKind::AppArg(matcher) => {
          let mut ac = ValEnv::default();
          for arm in matcher {
            if pat_match(&mut ac, cx, arm.pat, &val) {
              st.env = frame.env;
              st.env.val.extend(ac);
              return Step::exp(arm.exp);
            }
          }
          todo!("non-exhaustive fn match")
        }
        FrameKind::Raise => Step::Raise(val),
        // handle wasn't needed, as head didn't raise
        FrameKind::Handle(_) => Step::Val(val),
      },
    },
    Step::Raise(val) => match st.frames.pop() {
      // unhandled exception
      None => Step::Raise(val),
      Some(frame) => match frame.kind {
        FrameKind::Handle(matcher) => {
          let mut ac = ValEnv::default();
          for arm in matcher {
            if pat_match(&mut ac, cx, arm.pat, &val) {
              st.env = frame.env;
              st.env.val.extend(ac);
              return Step::exp(arm.exp);
            }
          }
          // handle didn't catch the exception. keep bubbling up
          Step::Raise(val)
        }
        // for all other frames, the exception continues to bubble up
        _ => Step::Raise(val),
      },
    },
  }
}

fn pat_match(ac: &mut ValEnv, cx: Cx<'_>, pat: sml_hir::PatIdx, val: &Val) -> bool {
  let pat = pat.expect("no pat");
  match (&cx.ars.pat[pat], val) {
    (sml_hir::Pat::Wild, _) => true,
    (sml_hir::Pat::Con(path, pat_arg), _) => match &cx.pat[pat] {
      IdStatus::Con => {
        let con_kind = ConKind::Dat(path.last().clone());
        pat_match_con(ac, cx, con_kind, *pat_arg, val)
      }
      IdStatus::Exn(exn) => {
        let con_kind = ConKind::Exn(path.last().clone(), *exn);
        pat_match_con(ac, cx, con_kind, *pat_arg, val)
      }
      IdStatus::Val => {
        assert!(path.prefix().is_empty());
        assert!(pat_arg.is_none());
        ac.insert(path.last().clone(), val.clone());
        true
      }
    },
    (_, Val::Closure(_, _)) => unreachable!("match non-(Wild or Con) with Closure"),
    (sml_hir::Pat::SCon(pat_sc), Val::SCon(val_sc)) => match (pat_sc, val_sc) {
      (SCon::Real(_), _) => unreachable!("Real pattern"),
      (_, SCon::Real(_)) => unreachable!("match non-(Wild or Con) with Real"),
      (SCon::Int(pat_int), SCon::Int(val_int)) => pat_int == val_int,
      (SCon::Word(pat_word), SCon::Word(val_word)) => pat_word == val_word,
      (SCon::Char(pat_char), SCon::Char(val_char)) => pat_char == val_char,
      (SCon::String(pat_str), SCon::String(val_str)) => pat_str == val_str,
      (SCon::Int(_) | SCon::Word(_) | SCon::Char(_) | SCon::String(_), _) => {
        unreachable!("SCon types do not match")
      }
    },
    (sml_hir::Pat::SCon(_), Val::Con(_) | Val::Record(_)) => {
      unreachable!("match SCon with (Con or Record")
    }
    (sml_hir::Pat::Record { rows: pat_rows, allows_other: _ }, Val::Record(val_rows)) => {
      pat_rows.iter().all(|(lab, pat)| pat_match(ac, cx, *pat, &val_rows[lab]))
    }
    (sml_hir::Pat::Record { .. }, Val::SCon(_) | Val::Con(_)) => {
      unreachable!("match Record with (SCon or Con")
    }
    (sml_hir::Pat::Typed(pat, _), _) => pat_match(ac, cx, *pat, val),
    (sml_hir::Pat::As(name, pat), val) => {
      ac.insert(name.clone(), val.clone());
      pat_match(ac, cx, *pat, val)
    }
    (sml_hir::Pat::Or(or_pat), val) => {
      let mut or_ac = ValEnv::default();
      for &pat in std::iter::once(&or_pat.first).chain(or_pat.rest.iter()) {
        if !pat_match(&mut or_ac, cx, pat, val) {
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

fn pat_match_con(
  ac: &mut ValEnv,
  cx: Cx<'_>,
  con_kind: ConKind,
  pat_arg: Option<sml_hir::PatIdx>,
  val: &Val,
) -> bool {
  let con = match val {
    Val::Con(x) => x,
    _ => unreachable!("match Con with non-Con"),
  };
  let same_con = match (con_kind, &con.kind) {
    (ConKind::Dat(d1), ConKind::Dat(d2)) => d1 == *d2,
    (ConKind::Exn(_, e1), ConKind::Exn(_, e2)) => e1 == *e2,
    (ConKind::Dat(_), ConKind::Exn(_, _)) | (ConKind::Exn(_, _), ConKind::Dat(_)) => false,
  };
  if !same_con {
    return false;
  }
  match (pat_arg, &con.arg) {
    (None, None) => true,
    (Some(pat_arg), Some(val_arg)) => pat_match(ac, cx, pat_arg, val_arg.as_ref()),
    (Some(_), None) => unreachable!("pat Con has arg but val does not"),
    (None, Some(_)) => unreachable!("pat Con has no arg but val does"),
  }
}
