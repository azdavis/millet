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

fn frame<'a, I>(mut iter: I, ars: &'a sml_hir::Arenas, f: &mut fmt::Formatter<'_>) -> fmt::Result
where
  I: Iterator<Item = &'a Frame>,
{
  let frame = match iter.next() {
    Some(x) => x,
    None => return Ok(()),
  };
  match &frame.kind {
    FrameKind::Record(vs, lab, es) => {
      f.write_str("{ ")?;
      fmt_util::comma_seq(f, vs.iter().map(|(lab, val)| ValRowDisplay { lab, val, ars }))?;
      fmt::Display::fmt(lab, f)?;
      f.write_str(" = ")?;
      // TODO next frame/step
      f.write_str("_")?;
      if !es.is_empty() {
        f.write_str(", ")?;
      }
      fmt_util::comma_seq(f, es.iter().map(|&(ref lab, exp)| ExpRowDisplay { lab, exp, ars }))?;
      f.write_str(" }")?;
      Ok(())
    }
    FrameKind::AppFunc(exp) => {
      // TODO next frame/step
      f.write_str("_")?;
      f.write_str(" ")?;
      fmt::Display::fmt(&ExpDisplay { this: *exp, ars }, f)?;
      Ok(())
    }
    FrameKind::AppArg(_) => todo!(),
    FrameKind::Raise => todo!(),
    FrameKind::Handle(_) => todo!(),
    FrameKind::Let(_, _) => todo!(),
    FrameKind::ValBind(_) => todo!(),
    FrameKind::Local(_, _) => todo!(),
  }
}

struct ValDisplay<'a> {
  this: &'a Val,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ValDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.this {
      Val::SCon(scon) => scon.fmt(f),
      Val::Con(con) => ConDisplay { this: con, ars: self.ars }.fmt(f),
      Val::Record(vs) => {
        f.write_str("{ ")?;
        fmt_util::comma_seq(
          f,
          vs.iter().map(|(lab, val)| ValRowDisplay { lab, val, ars: self.ars }),
        )?;
        f.write_str(" }")?;
        Ok(())
      }
      Val::Closure(_, arms) => {
        fmt_util::sep_seq(f, " | ", arms.iter().map(|this| ArmDisplay { this, ars: self.ars }))
      }
    }
  }
}

struct ValRowDisplay<'a> {
  lab: &'a Lab,
  val: &'a Val,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ValRowDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.lab.fmt(f)?;
    f.write_str(" = ")?;
    ValDisplay { this: self.val, ars: self.ars }.fmt(f)?;
    Ok(())
  }
}

struct ArmDisplay<'a> {
  this: &'a sml_hir::Arm,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ArmDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    PatDisplay { this: self.this.pat, ars: self.ars }.fmt(f)?;
    f.write_str(" => ")?;
    ExpDisplay { this: self.this.exp, ars: self.ars }.fmt(f)?;
    Ok(())
  }
}

struct ExpDisplay<'a> {
  this: sml_hir::ExpIdx,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ExpDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.ars.exp[self.this.ok_or(fmt::Error)?] {
      sml_hir::Exp::Hole => f.write_str("_"),
      sml_hir::Exp::SCon(scon) => scon.fmt(f),
      sml_hir::Exp::Path(path) => path.fmt(f),
      sml_hir::Exp::Record(rows) => {
        f.write_str("{ ")?;
        fmt_util::comma_seq(
          f,
          rows.iter().map(|&(ref lab, exp)| ExpRowDisplay { lab, exp, ars: self.ars }),
        )?;
        f.write_str(" }")?;
        Ok(())
      }
      sml_hir::Exp::Let(_, _) => todo!(),
      sml_hir::Exp::App(_, _) => todo!(),
      sml_hir::Exp::Handle(_, _) => todo!(),
      sml_hir::Exp::Raise(_) => todo!(),
      sml_hir::Exp::Fn(_, _) => todo!(),
      sml_hir::Exp::Typed(_, _) => todo!(),
    }
  }
}

struct ExpRowDisplay<'a> {
  lab: &'a Lab,
  exp: sml_hir::ExpIdx,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ExpRowDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.lab.fmt(f)?;
    f.write_str(" = ")?;
    ExpDisplay { this: self.exp, ars: self.ars }.fmt(f)?;
    Ok(())
  }
}

struct PatDisplay<'a> {
  this: sml_hir::PatIdx,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for PatDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.ars.pat[self.this.ok_or(fmt::Error)?] {
      sml_hir::Pat::Wild => f.write_str("_"),
      sml_hir::Pat::SCon(scon) => scon.fmt(f),
      sml_hir::Pat::Con(_, _) => todo!(),
      sml_hir::Pat::Record { .. } => todo!(),
      sml_hir::Pat::Typed(_, _) => todo!(),
      sml_hir::Pat::As(_, _) => todo!(),
      sml_hir::Pat::Or(_) => todo!(),
    }
  }
}

struct ConDisplay<'a> {
  this: &'a Con,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ConDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.this.kind {
      ConKind::Dat(name) | ConKind::Exn(name, _) => name.fmt(f)?,
    }
    if let Some(val) = &self.this.arg {
      // TODO prec
      f.write_str("(")?;
      ValDisplay { this: val.as_ref(), ars: self.ars }.fmt(f)?;
      f.write_str(")")?;
    }
    Ok(())
  }
}
