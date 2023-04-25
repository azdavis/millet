//! Stepping a stack machine.

use crate::pat_match;
use crate::types::{Closure, Con, ConKind, Cx, Env, Frame, FrameKind, St, Step, Val, ValEnv};
use fast_hash::FxHashSet;
use sml_hir::Lab;
use sml_statics_types::info::IdStatus;
use std::collections::BTreeMap;
use str_util::Name;

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
        let is_tuple = exp_rows.len() != 1
          && exp_rows.iter().enumerate().all(|(idx, (lab, _))| Lab::tuple(idx) == *lab);
        exp_rows.reverse();
        match exp_rows.pop() {
          None => Step::Val(Val::Record(BTreeMap::new())),
          Some((lab, exp)) => {
            st.push_with_cur_env(FrameKind::Record(is_tuple, BTreeMap::new(), lab, exp_rows));
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
      sml_hir::Exp::Fn(matcher, _) => {
        let clos =
          Closure { env: st.env.clone(), this: FxHashSet::default(), matcher: matcher.clone() };
        Step::Val(Val::Closure(clos))
      }
      sml_hir::Exp::Typed(exp, _) => Step::exp(*exp),
    },
    Step::Val(val) => match st.frames.pop() {
      // done evaluating
      None => Step::Val(val),
      Some(frame) => match frame.kind {
        FrameKind::Record(is_tuple, mut val_rows, lab, mut exp_rows) => {
          assert!(val_rows.insert(lab, val).is_none());
          match exp_rows.pop() {
            None => Step::Val(Val::Record(val_rows)),
            Some((lab, exp)) => {
              st.env = frame.env;
              st.push_with_cur_env(FrameKind::Record(is_tuple, val_rows, lab, exp_rows));
              Step::exp(exp)
            }
          }
        }
        FrameKind::AppFunc(arg) => match val {
          Val::Closure(clos) => {
            st.env = frame.env;
            let mut env = clos.env.clone();
            // recursion!
            for name in &clos.this {
              env.val.insert(name.clone(), Val::Closure(clos.clone()));
            }
            st.frames.push(Frame::new(env, FrameKind::AppClosureArg(clos.matcher)));
            Step::exp(arg)
          }
          Val::Con(con) => {
            assert!(con.arg.is_none(), "Con already has arg");
            st.env = frame.env;
            st.push_with_cur_env(FrameKind::AppConArg(con.kind));
            Step::exp(arg)
          }
          _ => unreachable!("AppFunc not Closure or Con"),
        },
        FrameKind::AppClosureArg(matcher) => {
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
        FrameKind::AppConArg(kind) => Step::Val(Val::Con(Con { kind, arg: Some(Box::new(val)) })),
        FrameKind::Raise => match val {
          Val::Con(con) => Step::Raise(con.try_into().expect("Raise Con but not Exception")),
          _ => unreachable!("Raise not Con"),
        },
        // handle wasn't needed, as head didn't raise
        FrameKind::Handle(_) => Step::Val(val),
        FrameKind::ValBind(recursive, pat, mut val_binds) => {
          let mut ac = ValEnv::default();
          if recursive {
            let mut this = FxHashSet::<Name>::default();
            rec_fn_names(cx.ars, &mut this, pat);
            let mut clos = match val {
              Val::Closure(x) => x,
              _ => unreachable!("val rec value must be Closure"),
            };
            assert!(clos.this.is_empty());
            clos.this = this.clone();
            for name in this {
              ac.insert(name, Val::Closure(clos.clone()));
            }
          } else if !pat_match::get(&mut ac, cx, pat, &val) {
            return Step::Raise(cx.bind_exn());
          }
          st.env = frame.env;
          st.env.val.extend(ac);
          match val_binds.pop() {
            Some(vb) => {
              st.push_with_cur_env(FrameKind::ValBind(vb.rec, vb.pat, val_binds));
              Step::exp(vb.exp)
            }
            None => step_dec(st),
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
        let mut val_binds = val_binds.clone();
        val_binds.reverse();
        let vb = val_binds.pop().unwrap();
        st.push_with_cur_env(FrameKind::ValBind(vb.rec, vb.pat, val_binds));
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
    // done with a dec
    Step::DecDone => {
      assert!(st.frames.is_empty(), "can't be done but still have frames");
      Step::DecDone
    }
  }
}

fn step_dec(st: &mut St) -> Step {
  while let Some(frame) = st.frames.pop() {
    match frame.kind {
      FrameKind::Record(_, _, _, _)
      | FrameKind::AppFunc(_)
      | FrameKind::AppClosureArg(_)
      | FrameKind::AppConArg(_)
      | FrameKind::Raise
      | FrameKind::Handle(_)
      | FrameKind::ValBind(_, _, _) => unreachable!("bad surrounding frame for Dec"),
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
  Step::DecDone
}

fn rec_fn_names(ars: &sml_hir::Arenas, ac: &mut FxHashSet<Name>, pat: sml_hir::PatIdx) {
  match &ars.pat[pat.expect("no pat")] {
    sml_hir::Pat::Wild => {}
    sml_hir::Pat::SCon(_) => unreachable!("SCon pat cannot match fn"),
    sml_hir::Pat::Con(path, argument) => {
      assert!(argument.is_none(), "Con pat with arg cannot match fn");
      assert!(path.prefix().is_empty(), "Con pat cannot match fn");
      ac.insert(path.last().clone());
    }
    sml_hir::Pat::Record { .. } => unreachable!("Record pat cannot match fn"),
    sml_hir::Pat::Typed(pat, _) => rec_fn_names(ars, ac, *pat),
    sml_hir::Pat::As(name, pat) => {
      ac.insert(name.clone());
      rec_fn_names(ars, ac, *pat);
    }
    sml_hir::Pat::Or(_) => unreachable!("Or pat should have been denied with unreachable pattern"),
  }
}
