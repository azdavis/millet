//! Displaying some types.

#![allow(dead_code)]

use crate::dynamics::Dynamics;
use crate::types::{Con, ConKind, Frame, FrameKind, Val};
use sml_hir::Lab;
use std::fmt;

impl fmt::Display for Dynamics<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Dynamics { .. }")
  }
}

fn frame<'a, I>(mut iter: I, f: &mut fmt::Formatter<'_>) -> fmt::Result
where
  I: Iterator<Item = &'a Frame>,
{
  let frame = match iter.next() {
    Some(x) => x,
    None => return Ok(()),
  };
  match &frame.kind {
    FrameKind::Record(vs, lab, _) => {
      f.write_str("{ ")?;
      fmt_util::comma_seq(f, vs.iter().map(|(lab, val)| ValRowDisplay { lab, val }))?;
      fmt::Display::fmt(lab, f)?;
      f.write_str(" = ")?;
      // TODO next frame/step
      f.write_str("_")?;
      // TODO es
      f.write_str(" }")?;
      Ok(())
    }
    FrameKind::AppFunc(_) => todo!(),
    FrameKind::AppArg(_) => todo!(),
    FrameKind::Raise => todo!(),
    FrameKind::Handle(_) => todo!(),
    FrameKind::Let(_, _) => todo!(),
    FrameKind::ValBind(_) => todo!(),
    FrameKind::Local(_, _) => todo!(),
  }
}

impl fmt::Display for Val {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Val::SCon(scon) => scon.fmt(f),
      Val::Con(con) => con.fmt(f),
      Val::Record(vs) => {
        f.write_str("{ ")?;
        fmt_util::comma_seq(f, vs.iter().map(|(lab, val)| ValRowDisplay { lab, val }))?;
        f.write_str(" }")?;
        Ok(())
      }
      Val::Closure(_, _) => todo!(),
    }
  }
}

struct ValRowDisplay<'a> {
  lab: &'a Lab,
  val: &'a Val,
}

impl fmt::Display for ValRowDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.lab.fmt(f)?;
    f.write_str(" = ")?;
    self.val.fmt(f)?;
    Ok(())
  }
}

impl fmt::Display for Con {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.kind {
      ConKind::Dat(name) | ConKind::Exn(name, _) => name.fmt(f)?,
    }
    if let Some(x) = &self.arg {
      // TODO prec
      f.write_str("(")?;
      x.fmt(f)?;
      f.write_str(")")?;
    }
    Ok(())
  }
}
