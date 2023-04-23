//! Stepping a stack machine.

use crate::pat_match;
use crate::types::{Con, ConKind, Cx, Env, Frame, FrameKind, St, Step, Val, ValEnv};
use sml_statics_types::info::IdStatus;
use std::collections::BTreeMap;

/// i think this is called a "stack machine". it is NOT recursive.
#[allow(clippy::too_many_lines)]
pub(crate) fn step(st: &mut St, cx: Cx<'_>, s: Step) -> Step {
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
          let env = st.env.get(path.prefix()).expect("no prefix");
          Step::Val(env.val[path.last()].clone())
        }
      },
      sml_hir::Exp::Record(exp_rows) => {
        let mut exp_rows = exp_rows.clone();
        exp_rows.reverse();
        match exp_rows.pop() {
          None => Step::Val(Val::Record(BTreeMap::new())),
          Some((lab, exp)) => {
            st.push_with_cur_env(FrameKind::Record(BTreeMap::new(), lab, exp_rows));
            Step::exp(exp)
          }
        }
      }
      sml_hir::Exp::Let(decs, exp) => {
        let mut decs = decs.clone();
        decs.reverse();
        st.push_with_cur_env(FrameKind::Let(decs, *exp));
        step_dec(st)
      }
      sml_hir::Exp::App(func, arg) => {
        st.push_with_cur_env(FrameKind::AppFunc(*arg));
        Step::exp(*func)
      }
      sml_hir::Exp::Handle(exp, matcher) => {
        st.push_with_cur_env(FrameKind::Handle(matcher.clone()));
        Step::exp(*exp)
      }
      sml_hir::Exp::Raise(exp) => {
        // don't care about the env for raise
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
        FrameKind::Record(mut val_rows, lab, mut exp_rows) => {
          assert!(val_rows.insert(lab, val).is_none());
          match exp_rows.pop() {
            None => Step::Val(Val::Record(val_rows)),
            Some((lab, exp)) => {
              st.env = frame.env;
              st.push_with_cur_env(FrameKind::Record(val_rows, lab, exp_rows));
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
            if pat_match::get(&mut ac, cx, arm.pat, &val) {
              st.env = frame.env;
              st.env.val.extend(ac);
              return Step::exp(arm.exp);
            }
          }
          Step::Raise(cx.match_exn())
        }
        FrameKind::Raise => match val {
          Val::Con(con) => Step::Raise(con.try_into().expect("Raise Con but not Exception")),
          _ => unreachable!("Raise not Con"),
        },
        // handle wasn't needed, as head didn't raise
        FrameKind::Handle(_) => Step::Val(val),
        FrameKind::ValBind(pat) => {
          let mut ac = ValEnv::default();
          if !pat_match::get(&mut ac, cx, pat, &val) {
            return Step::Raise(cx.bind_exn());
          }
          st.env = frame.env;
          st.env.val.extend(ac);
          let frame = st.frames.pop().expect("ValBind with no frame");
          let (mut decs, exp) = match frame.kind {
            FrameKind::Let(d, e) => (d, e),
            _ => unreachable!("ValBind frame not Let"),
          };
          match decs.pop() {
            Some(dec) => {
              st.frames.push(Frame::new(frame.env, FrameKind::Let(decs, exp)));
              Step::Dec(dec)
            }
            None => Step::exp(exp),
          }
        }
        FrameKind::Let(_, _) | FrameKind::Local(_, _) | FrameKind::In(_) => {
          unreachable!("bad surrounding frame for Val")
        }
      },
    },
    Step::Raise(exception) => match st.frames.pop() {
      // unhandled exception
      None => Step::Raise(exception),
      Some(frame) => match frame.kind {
        FrameKind::Handle(matcher) => {
          let mut ac = ValEnv::default();
          let val = Val::Con(exception.clone().into());
          for arm in matcher {
            if pat_match::get(&mut ac, cx, arm.pat, &val) {
              st.env = frame.env;
              st.env.val.extend(ac);
              return Step::exp(arm.exp);
            }
          }
          // handle didn't catch the exception. keep bubbling up
          Step::Raise(exception)
        }
        // for all other frames, the exception continues to bubble up
        _ => Step::Raise(exception),
      },
    },
    Step::Dec(dec) => match &cx.ars.dec[dec] {
      sml_hir::Dec::Val(_, val_binds) => {
        let mut val_bind = val_binds.clone().into_iter();
        let vb = val_bind.next().unwrap();
        st.push_with_cur_env(FrameKind::ValBind(vb.pat));
        Step::exp(vb.exp)
      }
      sml_hir::Dec::Ty(_)
      | sml_hir::Dec::Datatype(_, _)
      | sml_hir::Dec::DatatypeCopy(_, _)
      | sml_hir::Dec::Abstype(_, _, _)
      | sml_hir::Dec::Exception(_)
      | sml_hir::Dec::Open(_) => step_dec(st),
      sml_hir::Dec::Local(local_decs, in_decs) => {
        let mut local_decs = local_decs.clone();
        let mut in_decs = in_decs.clone();
        local_decs.reverse();
        in_decs.reverse();
        st.push_with_cur_env(FrameKind::Local(local_decs, in_decs));
        step_dec(st)
      }
    },
  }
}

fn step_dec(st: &mut St) -> Step {
  while let Some(frame) = st.frames.pop() {
    match frame.kind {
      FrameKind::Record(_, _, _)
      | FrameKind::AppFunc(_)
      | FrameKind::AppArg(_)
      | FrameKind::Raise
      | FrameKind::Handle(_)
      | FrameKind::ValBind(_) => unreachable!("bad surrounding frame for Dec"),
      FrameKind::Let(mut decs, exp) => match decs.pop() {
        None => return Step::exp(exp),
        Some(dec) => {
          st.push_with_cur_env(FrameKind::Let(decs, exp));
          return Step::Dec(dec);
        }
      },
      FrameKind::Local(mut local_decs, in_decs) => match local_decs.pop() {
        // return to top of loop
        None => st.push_with_cur_env(FrameKind::In(in_decs)),
        Some(dec) => {
          st.push_with_cur_env(FrameKind::Local(local_decs, in_decs));
          return Step::Dec(dec);
        }
      },
      FrameKind::In(mut in_decs) => match in_decs.pop() {
        // keep popping
        None => {}
        Some(dec) => {
          st.push_with_cur_env(FrameKind::In(in_decs));
          return Step::Dec(dec);
        }
      },
    }
  }
  unreachable!("ran out of frames")
}
