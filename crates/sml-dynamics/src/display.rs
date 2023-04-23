//! Displaying some types.

// TODO fix prec everywhere

use crate::dynamics::Dynamics;
use crate::types::{Con, ConKind, Exception, Frame, FrameKind, Step, Val};
use sml_hir::Lab;
use std::fmt::{self, Display as _};

impl fmt::Display for Dynamics<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut iter = self.st.frames.iter();
    go(&mut iter, self.cx.ars, self.step.as_ref().ok_or(fmt::Error)?, f)
  }
}

fn go<'a, I>(
  iter: &mut I,
  ars: &sml_hir::Arenas,
  step: &Step,
  f: &mut fmt::Formatter<'_>,
) -> fmt::Result
where
  I: Iterator<Item = &'a Frame>,
{
  match iter.next() {
    Some(frame) => match &frame.kind {
      FrameKind::Record(vs, lab, es) => {
        f.write_str("{ ")?;
        fmt_util::comma_seq(f, vs.iter().map(|(lab, val)| ValRowDisplay { lab, val, ars }))?;
        lab.fmt(f)?;
        f.write_str(" = ")?;
        go(iter, ars, step, f)?;
        if !es.is_empty() {
          f.write_str(", ")?;
        }
        fmt_util::comma_seq(f, es.iter().map(|&(ref lab, exp)| ExpRowDisplay { lab, exp, ars }))?;
        f.write_str(" }")
      }
      FrameKind::AppFunc(exp) => {
        go(iter, ars, step, f)?;
        f.write_str(" ")?;
        ExpDisplay { exp: exp.ok_or(fmt::Error)?, ars }.fmt(f)
      }
      FrameKind::AppArg(matcher) => {
        FnDisplay { matcher, ars }.fmt(f)?;
        f.write_str(" ")?;
        go(iter, ars, step, f)
      }
      FrameKind::Raise => {
        f.write_str("raise ")?;
        go(iter, ars, step, f)
      }
      FrameKind::Handle(matcher) => {
        go(iter, ars, step, f)?;
        f.write_str(" handle ")?;
        fmt_util::sep_seq(f, " | ", matcher.iter().map(|arm| ArmDisplay { arm, ars }))
      }
      FrameKind::Let(decs, exp) => {
        f.write_str("let ")?;
        go(iter, ars, step, f)?;
        fmt_util::sep_seq(f, " ", decs.iter().rev().map(|&dec| DecDisplay { dec, ars }))?;
        f.write_str(" in ")?;
        ExpDisplay { exp: exp.ok_or(fmt::Error)?, ars }.fmt(f)?;
        f.write_str(" end")
      }
      FrameKind::ValBind(pat, val_binds) => {
        f.write_str("val ")?;
        PatDisplay { pat: pat.ok_or(fmt::Error)?, ars }.fmt(f)?;
        f.write_str(" = ")?;
        go(iter, ars, step, f)?;
        let val_binds = val_binds.iter().rev().map(|&val_bind| ValBindDisplay { val_bind, ars });
        fmt_util::sep_seq(f, " ", val_binds)
      }
      FrameKind::Local(local_decs, in_decs) => {
        f.write_str("local ")?;
        go(iter, ars, step, f)?;
        fmt_util::sep_seq(f, " ", local_decs.iter().rev().map(|&dec| DecDisplay { dec, ars }))?;
        f.write_str(" in ")?;
        fmt_util::sep_seq(f, " ", in_decs.iter().rev().map(|&dec| DecDisplay { dec, ars }))?;
        f.write_str(" end")
      }
      FrameKind::In(in_decs) => {
        f.write_str("local in")?;
        go(iter, ars, step, f)?;
        fmt_util::sep_seq(f, " ", in_decs.iter().rev().map(|&dec| DecDisplay { dec, ars }))?;
        f.write_str("end")
      }
    },
    None => match step {
      Step::Exp(exp) => ExpDisplay { exp: *exp, ars }.fmt(f),
      Step::Val(val) => ValDisplay { val, ars }.fmt(f),
      Step::Raise(exception) => {
        f.write_str("raise ")?;
        ExceptionDisplay { exception, ars }.fmt(f)
      }
      Step::Dec(dec) => DecDisplay { dec: *dec, ars }.fmt(f),
    },
  }
}

struct ValDisplay<'a> {
  val: &'a Val,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ValDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.val {
      Val::SCon(scon) => scon.fmt(f),
      Val::Con(con) => ConDisplay { con, ars: self.ars }.fmt(f),
      Val::Record(vs) => {
        f.write_str("{ ")?;
        let rows = vs.iter().map(|(lab, val)| ValRowDisplay { lab, val, ars: self.ars });
        fmt_util::comma_seq(f, rows)?;
        f.write_str(" }")
      }
      Val::Closure(_, matcher) => FnDisplay { matcher, ars: self.ars }.fmt(f),
    }
  }
}

struct ValBindDisplay<'a> {
  val_bind: sml_hir::ValBind,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ValBindDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("val ")?;
    PatDisplay { pat: self.val_bind.pat.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)?;
    f.write_str(" = ")?;
    ExpDisplay { exp: self.val_bind.exp.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)
  }
}
struct FnDisplay<'a> {
  matcher: &'a [sml_hir::Arm],
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for FnDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("fn ")?;
    let arms = self.matcher.iter().map(|arm| ArmDisplay { arm, ars: self.ars });
    fmt_util::sep_seq(f, " | ", arms)
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
    ValDisplay { val: self.val, ars: self.ars }.fmt(f)
  }
}

struct ArmDisplay<'a> {
  arm: &'a sml_hir::Arm,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ArmDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    PatDisplay { pat: self.arm.pat.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)?;
    f.write_str(" => ")?;
    ExpDisplay { exp: self.arm.exp.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)
  }
}

struct ExpDisplay<'a> {
  exp: sml_hir::la_arena::Idx<sml_hir::Exp>,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ExpDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.ars.exp[self.exp] {
      sml_hir::Exp::Hole => f.write_str("_"),
      sml_hir::Exp::SCon(scon) => scon.fmt(f),
      sml_hir::Exp::Path(path) => path.fmt(f),
      sml_hir::Exp::Record(rows) => {
        f.write_str("{ ")?;
        let rows = rows.iter().map(|&(ref lab, exp)| ExpRowDisplay { lab, exp, ars: self.ars });
        fmt_util::comma_seq(f, rows)?;
        f.write_str(" }")
      }
      sml_hir::Exp::Let(decs, exp) => {
        f.write_str("let ")?;
        fmt_util::sep_seq(f, " ", decs.iter().rev().map(|&dec| DecDisplay { dec, ars: self.ars }))?;
        f.write_str(" in ")?;
        ExpDisplay { exp: exp.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)?;
        f.write_str(" end")
      }
      sml_hir::Exp::App(func, argument) => {
        ExpDisplay { exp: func.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)?;
        f.write_str(" ")?;
        ExpDisplay { exp: argument.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)
      }
      sml_hir::Exp::Handle(exp, matcher) => {
        ExpDisplay { exp: exp.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)?;
        f.write_str(" handle ")?;
        fmt_util::sep_seq(f, " | ", matcher.iter().map(|arm| ArmDisplay { arm, ars: self.ars }))
      }
      sml_hir::Exp::Raise(exp) => {
        f.write_str("raise ")?;
        ExpDisplay { exp: exp.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)
      }
      sml_hir::Exp::Fn(matcher, _) => FnDisplay { matcher, ars: self.ars }.fmt(f),
      sml_hir::Exp::Typed(exp, _) => {
        ExpDisplay { exp: exp.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)
      }
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
    ExpDisplay { exp: self.exp.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)
  }
}

struct PatDisplay<'a> {
  pat: sml_hir::la_arena::Idx<sml_hir::Pat>,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for PatDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.ars.pat[self.pat] {
      sml_hir::Pat::Wild => f.write_str("_"),
      sml_hir::Pat::SCon(scon) => scon.fmt(f),
      sml_hir::Pat::Con(path, arg) => {
        path.last().fmt(f)?;
        if let Some(arg) = arg {
          PatDisplay { pat: arg.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)?;
        }
        Ok(())
      }
      sml_hir::Pat::Record { rows, allows_other } => {
        f.write_str("{ ")?;
        let rows = rows.iter().map(|&(ref lab, pat)| PatRowDisplay { lab, pat, ars: self.ars });
        fmt_util::comma_seq(f, rows)?;
        if *allows_other {
          f.write_str(", ...")?;
        }
        f.write_str(" }")
      }
      sml_hir::Pat::Typed(pat, _) => {
        PatDisplay { pat: pat.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)
      }
      sml_hir::Pat::As(name, pat) => {
        name.fmt(f)?;
        f.write_str(" as ")?;
        PatDisplay { pat: pat.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)
      }
      sml_hir::Pat::Or(or_pat) => {
        PatDisplay { pat: or_pat.first.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)?;
        for pat in &or_pat.rest {
          f.write_str(" | ")?;
          PatDisplay { pat: pat.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)?;
        }
        Ok(())
      }
    }
  }
}

struct PatRowDisplay<'a> {
  lab: &'a Lab,
  pat: sml_hir::PatIdx,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for PatRowDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.lab.fmt(f)?;
    f.write_str(" = ")?;
    PatDisplay { pat: self.pat.ok_or(fmt::Error)?, ars: self.ars }.fmt(f)
  }
}

struct ConDisplay<'a> {
  con: &'a Con,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ConDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.con.kind {
      ConKind::Dat(name) | ConKind::Exn(name, _) => name.fmt(f)?,
    }
    if let Some(val) = &self.con.arg {
      ValDisplay { val: val.as_ref(), ars: self.ars }.fmt(f)?;
    }
    Ok(())
  }
}

struct ExceptionDisplay<'a> {
  exception: &'a Exception,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for ExceptionDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.exception.name.fmt(f)?;
    if let Some(val) = &self.exception.arg {
      ValDisplay { val: val.as_ref(), ars: self.ars }.fmt(f)?;
    }
    Ok(())
  }
}

struct DecDisplay<'a> {
  dec: sml_hir::DecIdx,
  ars: &'a sml_hir::Arenas,
}

impl fmt::Display for DecDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.ars.dec[self.dec] {
      sml_hir::Dec::Val(_, val_binds) => {
        let val_binds =
          val_binds.iter().map(|&val_bind| ValBindDisplay { val_bind, ars: self.ars });
        fmt_util::sep_seq(f, " ", val_binds)
      }
      sml_hir::Dec::Ty(_) => f.write_str("type ..."),
      sml_hir::Dec::Datatype(_, _) | sml_hir::Dec::DatatypeCopy(_, _) => {
        f.write_str("datatype ...")
      }
      sml_hir::Dec::Abstype(_, _, _) => Err(fmt::Error),
      sml_hir::Dec::Exception(_) => f.write_str("exception ..."),
      sml_hir::Dec::Local(local_decs, in_decs) => {
        f.write_str("local ")?;
        fmt_util::sep_seq(f, " ", local_decs.iter().map(|&dec| DecDisplay { dec, ars: self.ars }))?;
        f.write_str(" in ")?;
        fmt_util::sep_seq(f, " ", in_decs.iter().map(|&dec| DecDisplay { dec, ars: self.ars }))?;
        f.write_str(" end")
      }
      sml_hir::Dec::Open(_) => f.write_str("open ..."),
    }
  }
}
