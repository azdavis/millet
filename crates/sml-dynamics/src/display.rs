//! Displaying some types.

use crate::dynamics::Dynamics;
use crate::types::{Con, Env, Exception, FrameKind, Step, Val};
use sml_hir::Lab;
use std::fmt;

impl fmt::Display for Dynamics<'_> {
  #[allow(clippy::too_many_lines)]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let ars = &self.cx.ars;
    if f.alternate() {
      f.write_str("(* env:\n")?;
      if let Some(frame) = self.st.frames.last() {
        EnvDisplay { env: &frame.env, ars, indent: 0 }.fmt(f)?;
      }
      f.write_str("*)\n")?;
    }
    let mut indent = 0usize;
    let mut prec = Prec::Min;
    let mut frame_prec = Vec::with_capacity(self.st.frames.len());
    for frame in &self.st.frames {
      frame_prec.push(prec);
      match &frame.kind {
        FrameKind::Handle(_) | FrameKind::DecSeq(_) | FrameKind::StrDecSeq(_) => {}
        FrameKind::AppFunc(_) => {
          if matches!(prec, Prec::Atomic) {
            f.write_str("(")?;
          }
          prec = Prec::Matcher;
        }
        FrameKind::Record(is_tuple, vs, lab, _) => {
          if *is_tuple {
            f.write_str("(")?;
            for val in vs.values() {
              ValDisplay { val, ars, prec: Prec::Min, indent }.fmt(f)?;
              f.write_str(", ")?;
            }
          } else {
            f.write_str("{ ")?;
            for (lab, val) in vs {
              ValRowDisplay { lab, val, ars, indent }.fmt(f)?;
              f.write_str(", ")?;
            }
            lab.fmt(f)?;
            f.write_str(" = ")?;
          }
          prec = Prec::Min;
        }
        FrameKind::AppClosureArg(matcher) => {
          if matches!(prec, Prec::Atomic) {
            f.write_str("(")?;
          }
          f.write_str("(")?;
          FnDisplay { matcher, ars, indent }.fmt(f)?;
          f.write_str(") ")?;
          prec = Prec::Atomic;
        }
        FrameKind::AppBuiltinArg(builtin) => {
          if matches!(prec, Prec::Atomic) {
            f.write_str("(")?;
          }
          f.write_str(builtin.as_str())?;
          f.write_str(" ")?;
          prec = Prec::Atomic;
        }
        FrameKind::AppConArg(name, _) => {
          if matches!(prec, Prec::Atomic) {
            f.write_str("(")?;
          }
          name.fmt(f)?;
          f.write_str(" ")?;
          prec = Prec::Atomic;
        }
        FrameKind::Raise => {
          if matches!(prec, Prec::Atomic) {
            f.write_str("(")?;
          }
          f.write_str("raise ")?;
        }
        FrameKind::Let(_, _) => {
          f.write_str("let")?;
          indent += 1;
          write_nl_indent(indent, f)?;
        }
        FrameKind::ValBind(recursive, pat, _) => {
          f.write_str("val ")?;
          if *recursive {
            f.write_str("rec ")?;
          }
          PatDisplay { pat: *pat, ars, atomic: false }.fmt(f)?;
          f.write_str(" = ")?;
          prec = Prec::Min;
        }
        FrameKind::Local(_, _) => {
          f.write_str("local")?;
          indent += 1;
          write_nl_indent(indent, f)?;
        }
        FrameKind::In(_) => {
          f.write_str("local in")?;
          indent += 1;
          write_nl_indent(indent, f)?;
        }
      }
    }
    match self.step.as_ref().ok_or(fmt::Error)? {
      Step::Exp(exp) => ExpDisplay { exp: Some(*exp), ars, prec, indent }.fmt(f)?,
      Step::Val(val) => ValDisplay { val, ars, prec, indent }.fmt(f)?,
      Step::Raise(exception) => {
        f.write_str("raise ")?;
        ExceptionDisplay { exception, ars, indent }.fmt(f)?;
      }
      Step::Dec(dec) => DecDisplay { dec: *dec, ars, indent }.fmt(f)?,
      Step::DecDone => {}
      Step::StrDec(str_dec) => StrDecDisplay { str_dec: *str_dec, ars, indent }.fmt(f)?,
    }
    for frame in self.st.frames.iter().rev() {
      let prec = frame_prec.pop().unwrap();
      match &frame.kind {
        FrameKind::Raise
        | FrameKind::AppClosureArg(_)
        | FrameKind::AppBuiltinArg(_)
        | FrameKind::AppConArg(_, _) => {
          if matches!(prec, Prec::Atomic) {
            f.write_str(")")?;
          }
        }
        FrameKind::Record(is_tuple, _, _, es) => {
          if !es.is_empty() {
            f.write_str(", ")?;
          }
          if *is_tuple {
            let rows = es.iter().map(|&(_, exp)| ExpDisplay { exp, ars, prec: Prec::Min, indent });
            fmt_util::comma_seq(f, rows.into_iter())?;
            f.write_str(")")?;
          } else {
            let rows = es.iter().map(|&(ref lab, exp)| ExpRowDisplay { lab, exp, ars, indent });
            fmt_util::comma_seq(f, rows)?;
            f.write_str(" }")?;
          }
        }
        FrameKind::AppFunc(exp) => {
          f.write_str(" ")?;
          ExpDisplay { exp: *exp, ars, prec: Prec::Atomic, indent }.fmt(f)?;
          if matches!(prec, Prec::Atomic) {
            f.write_str(")")?;
          }
        }
        FrameKind::Handle(matcher) => {
          f.write_str(" handle")?;
          write_nl_indent(indent + 1, f)?;
          MatcherDisplay { matcher: &matcher[..], ars, indent }.fmt(f)?;
        }
        FrameKind::Let(decs, exp) => {
          for &dec in decs.iter().rev() {
            write_nl_indent(indent, f)?;
            DecDisplay { dec, ars, indent }.fmt(f)?;
          }
          write_nl_indent(indent - 1, f)?;
          f.write_str("in")?;
          write_nl_indent(indent, f)?;
          ExpDisplay { exp: *exp, ars, prec: Prec::Min, indent }.fmt(f)?;
          indent -= 1;
          write_nl_indent(indent, f)?;
          f.write_str("end")?;
        }
        FrameKind::ValBind(_, _, val_binds) => {
          for &val_bind in val_binds.iter().rev() {
            write_nl_indent(indent, f)?;
            ValBindDisplay { val_bind, ars, first: false, indent }.fmt(f)?;
          }
        }
        FrameKind::Local(local_decs, in_decs) => {
          for &dec in local_decs.iter().rev() {
            write_nl_indent(indent, f)?;
            DecDisplay { dec, ars, indent }.fmt(f)?;
          }
          write_nl_indent(indent - 1, f)?;
          f.write_str("in")?;
          for &dec in in_decs.iter().rev() {
            write_nl_indent(indent, f)?;
            DecDisplay { dec, ars, indent }.fmt(f)?;
          }
          indent -= 1;
          write_nl_indent(indent, f)?;
          f.write_str("end")?;
        }
        FrameKind::In(in_decs) => {
          for &dec in in_decs.iter().rev() {
            write_nl_indent(indent, f)?;
            DecDisplay { dec, ars, indent }.fmt(f)?;
          }
          indent -= 1;
          write_nl_indent(indent, f)?;
          f.write_str("end")?;
        }
        FrameKind::DecSeq(decs) => {
          for &dec in decs.iter().rev() {
            write_nl_indent(indent, f)?;
            DecDisplay { dec, ars, indent }.fmt(f)?;
          }
        }
        FrameKind::StrDecSeq(str_decs) => {
          for &str_dec in str_decs.iter().rev() {
            write_nl_indent(indent, f)?;
            StrDecDisplay { str_dec, ars, indent }.fmt(f)?;
          }
        }
      }
    }
    assert!(frame_prec.is_empty());
    Ok(())
  }
}

// TODO have this derive Ord etc and remove ad-hoc matches!()
#[derive(Debug, Clone, Copy)]
enum Prec {
  Min,
  Matcher,
  App,
  Atomic,
}

fn write_nl_indent(indent: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
  if f.alternate() {
    f.write_str("\n")?;
    for _ in 0..indent {
      f.write_str("  ")?;
    }
  } else {
    f.write_str(" ")?;
  }
  Ok(())
}

fn nl_indent_sep<I, T>(indent: usize, f: &mut fmt::Formatter<'_>, mut iter: I) -> fmt::Result
where
  I: Iterator<Item = T>,
  T: fmt::Display,
{
  let fst = iter.next().ok_or(fmt::Error)?;
  fst.fmt(f)?;
  for x in iter {
    write_nl_indent(indent, f)?;
    x.fmt(f)?;
  }
  Ok(())
}

struct ValDisplay<'a> {
  val: &'a Val,
  ars: &'a sml_hir::Arenas,
  prec: Prec,
  indent: usize,
}

impl fmt::Display for ValDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.val {
      Val::SCon(scon) => scon.fmt(f),
      Val::Con(con) => {
        let con = ConDisplay {
          con,
          ars: self.ars,
          atomic: matches!(self.prec, Prec::Atomic),
          indent: self.indent,
        };
        con.fmt(f)
      }
      Val::Record(rows) => {
        let is_tuple =
          rows.len() != 1 && rows.iter().enumerate().all(|(idx, (lab, _))| Lab::tuple(idx) == *lab);
        if is_tuple {
          f.write_str("(")?;
          let rows = rows.iter().map(|(_, val)| ValDisplay {
            val,
            ars: self.ars,
            prec: Prec::Min,
            indent: self.indent,
          });
          fmt_util::comma_seq(f, rows)?;
          f.write_str(")")?;
        } else {
          f.write_str("{ ")?;
          let rows = rows.iter().map(|(lab, val)| ValRowDisplay {
            lab,
            val,
            ars: self.ars,
            indent: self.indent,
          });
          fmt_util::comma_seq(f, rows)?;
          f.write_str(" }")?;
        }
        Ok(())
      }
      Val::Closure(clos) => {
        let needs_paren = matches!(self.prec, Prec::App | Prec::Matcher | Prec::Atomic);
        if needs_paren {
          f.write_str("(")?;
        }
        FnDisplay { matcher: &clos.matcher, ars: self.ars, indent: self.indent }.fmt(f)?;
        if needs_paren {
          f.write_str(")")?;
        }
        Ok(())
      }
      Val::Builtin(builtin) => f.write_str(builtin.as_str()),
    }
  }
}

struct ValBindDisplay<'a> {
  val_bind: sml_hir::ValBind,
  ars: &'a sml_hir::Arenas,
  first: bool,
  indent: usize,
}

impl fmt::Display for ValBindDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(if self.first { "val " } else { "and " })?;
    if self.val_bind.rec {
      f.write_str("rec ")?;
    }
    PatDisplay { pat: self.val_bind.pat, ars: self.ars, atomic: false }.fmt(f)?;
    f.write_str(" = ")?;
    ExpDisplay { exp: self.val_bind.exp, ars: self.ars, prec: Prec::Min, indent: self.indent }
      .fmt(f)
  }
}

struct FnDisplay<'a> {
  matcher: &'a [sml_hir::Arm],
  ars: &'a sml_hir::Arenas,
  indent: usize,
}

impl fmt::Display for FnDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("fn ")?;
    MatcherDisplay { matcher: self.matcher, ars: self.ars, indent: self.indent }.fmt(f)
  }
}

struct ValRowDisplay<'a> {
  lab: &'a Lab,
  val: &'a Val,
  ars: &'a sml_hir::Arenas,
  indent: usize,
}

impl fmt::Display for ValRowDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.lab.fmt(f)?;
    f.write_str(" = ")?;
    ValDisplay { val: self.val, ars: self.ars, prec: Prec::Min, indent: self.indent }.fmt(f)
  }
}

struct MatcherDisplay<'a> {
  matcher: &'a [sml_hir::Arm],
  ars: &'a sml_hir::Arenas,
  indent: usize,
}

impl fmt::Display for MatcherDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut iter = self.matcher.iter();
    ArmDisplay { arm: iter.next().ok_or(fmt::Error)?, ars: self.ars, indent: self.indent + 1 }
      .fmt(f)?;
    for arm in iter {
      write_nl_indent(self.indent, f)?;
      f.write_str("| ")?;
      ArmDisplay { arm, ars: self.ars, indent: self.indent + 1 }.fmt(f)?;
    }
    Ok(())
  }
}

struct ArmDisplay<'a> {
  arm: &'a sml_hir::Arm,
  ars: &'a sml_hir::Arenas,
  indent: usize,
}

impl fmt::Display for ArmDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    PatDisplay { pat: self.arm.pat, ars: self.ars, atomic: false }.fmt(f)?;
    f.write_str(" => ")?;
    ExpDisplay { exp: self.arm.exp, ars: self.ars, prec: Prec::Min, indent: self.indent }.fmt(f)
  }
}

struct ExpDisplay<'a> {
  exp: sml_hir::ExpIdx,
  ars: &'a sml_hir::Arenas,
  prec: Prec,
  indent: usize,
}

impl fmt::Display for ExpDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.ars.exp[self.exp.ok_or(fmt::Error)?] {
      sml_hir::Exp::Hole => f.write_str("_"),
      sml_hir::Exp::SCon(scon) => scon.fmt(f),
      sml_hir::Exp::Path(path) => path.fmt(f),
      sml_hir::Exp::Record(rows) => {
        let is_tuple =
          rows.len() != 1 && rows.iter().enumerate().all(|(idx, (lab, _))| Lab::tuple(idx) == *lab);
        if is_tuple {
          f.write_str("(")?;
          let rows = rows.iter().map(|&(_, exp)| ExpDisplay {
            exp,
            ars: self.ars,
            prec: Prec::Min,
            indent: self.indent,
          });
          fmt_util::comma_seq(f, rows)?;
          f.write_str(")")
        } else {
          f.write_str("{ ")?;
          let rows = rows.iter().map(|&(ref lab, exp)| ExpRowDisplay {
            lab,
            exp,
            ars: self.ars,
            indent: self.indent,
          });
          fmt_util::comma_seq(f, rows)?;
          f.write_str(" }")
        }
      }
      sml_hir::Exp::Let(decs, exp) => {
        f.write_str("let")?;
        for &dec in decs.iter().rev() {
          write_nl_indent(self.indent + 1, f)?;
          DecDisplay { dec, ars: self.ars, indent: self.indent + 1 }.fmt(f)?;
        }
        write_nl_indent(self.indent, f)?;
        f.write_str("in")?;
        write_nl_indent(self.indent + 1, f)?;
        ExpDisplay { exp: *exp, ars: self.ars, prec: Prec::Min, indent: self.indent + 1 }.fmt(f)?;
        write_nl_indent(self.indent, f)?;
        f.write_str("end")
      }
      sml_hir::Exp::App(func, argument) => {
        let needs_paren = matches!(self.prec, Prec::Atomic);
        if needs_paren {
          f.write_str("(")?;
        }
        ExpDisplay { exp: *func, ars: self.ars, prec: Prec::App, indent: self.indent }.fmt(f)?;
        f.write_str(" ")?;
        ExpDisplay { exp: *argument, ars: self.ars, prec: Prec::Atomic, indent: self.indent }
          .fmt(f)?;
        if needs_paren {
          f.write_str(")")?;
        }
        Ok(())
      }
      sml_hir::Exp::Handle(exp, matcher) => {
        let needs_paren = matches!(self.prec, Prec::App | Prec::Atomic);
        if needs_paren {
          f.write_str("(")?;
        }
        ExpDisplay { exp: *exp, ars: self.ars, prec: Prec::Matcher, indent: self.indent }.fmt(f)?;
        f.write_str(" handle")?;
        write_nl_indent(self.indent + 1, f)?;
        MatcherDisplay { matcher: &matcher[..], ars: self.ars, indent: self.indent }.fmt(f)?;
        if needs_paren {
          f.write_str(")")?;
        }
        Ok(())
      }
      sml_hir::Exp::Raise(exp) => {
        let needs_paren = matches!(self.prec, Prec::Matcher | Prec::App | Prec::Atomic);
        if needs_paren {
          f.write_str("(")?;
        }
        f.write_str("raise ")?;
        ExpDisplay { exp: *exp, ars: self.ars, prec: Prec::Min, indent: self.indent }.fmt(f)?;
        if needs_paren {
          f.write_str(")")?;
        }
        Ok(())
      }
      sml_hir::Exp::Fn(matcher, _) => {
        let needs_paren = matches!(self.prec, Prec::Matcher | Prec::App | Prec::Atomic);
        if needs_paren {
          f.write_str("(")?;
        }
        FnDisplay { matcher, ars: self.ars, indent: self.indent }.fmt(f)?;
        if needs_paren {
          f.write_str(")")?;
        }
        Ok(())
      }
      sml_hir::Exp::Typed(exp, _, _) => {
        ExpDisplay { exp: *exp, ars: self.ars, prec: self.prec, indent: self.indent }.fmt(f)
      }
    }
  }
}

struct ExpRowDisplay<'a> {
  lab: &'a Lab,
  exp: sml_hir::ExpIdx,
  ars: &'a sml_hir::Arenas,
  indent: usize,
}

impl fmt::Display for ExpRowDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.lab.fmt(f)?;
    f.write_str(" = ")?;
    ExpDisplay { exp: self.exp, ars: self.ars, prec: Prec::Min, indent: self.indent }.fmt(f)
  }
}

struct PatDisplay<'a> {
  pat: sml_hir::PatIdx,
  ars: &'a sml_hir::Arenas,
  atomic: bool,
}

impl fmt::Display for PatDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.ars.pat[self.pat.ok_or(fmt::Error)?] {
      sml_hir::Pat::Wild => f.write_str("_"),
      sml_hir::Pat::SCon(scon) => scon.fmt(f),
      sml_hir::Pat::Con(path, arg) => {
        let needs_paren = self.atomic && arg.is_some();
        if needs_paren {
          f.write_str("(")?;
        }
        path.last().fmt(f)?;
        if let Some(pat) = *arg {
          f.write_str(" ")?;
          PatDisplay { pat, ars: self.ars, atomic: true }.fmt(f)?;
        }
        if needs_paren {
          f.write_str(")")?;
        }
        Ok(())
      }
      sml_hir::Pat::Record { rows, allows_other } => {
        let is_tuple = !allows_other
          && rows.len() != 1
          && rows.iter().enumerate().all(|(idx, (lab, _))| Lab::tuple(idx) == *lab);
        if is_tuple {
          f.write_str("(")?;
          let rows = rows.iter().map(|&(_, pat)| PatDisplay { pat, ars: self.ars, atomic: false });
          fmt_util::comma_seq(f, rows)?;
          f.write_str(")")
        } else {
          f.write_str("{ ")?;
          let rows = rows.iter().map(|&(ref lab, pat)| PatRowDisplay { lab, pat, ars: self.ars });
          fmt_util::comma_seq(f, rows)?;
          if *allows_other {
            f.write_str(", ...")?;
          }
          f.write_str(" }")
        }
      }
      sml_hir::Pat::Typed(pat, _) => {
        PatDisplay { pat: *pat, ars: self.ars, atomic: self.atomic }.fmt(f)
      }
      sml_hir::Pat::As(name, pat) => {
        if self.atomic {
          f.write_str("(")?;
        }
        name.fmt(f)?;
        f.write_str(" as ")?;
        PatDisplay { pat: *pat, ars: self.ars, atomic: false }.fmt(f)?;
        if self.atomic {
          f.write_str(")")?;
        }
        Ok(())
      }
      sml_hir::Pat::Or(or_pat) => {
        if self.atomic {
          f.write_str("(")?;
        }
        PatDisplay { pat: or_pat.first, ars: self.ars, atomic: false }.fmt(f)?;
        for &pat in &or_pat.rest {
          f.write_str(" | ")?;
          PatDisplay { pat, ars: self.ars, atomic: false }.fmt(f)?;
        }
        if self.atomic {
          f.write_str(")")?;
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
    PatDisplay { pat: self.pat, ars: self.ars, atomic: false }.fmt(f)
  }
}

struct ConDisplay<'a> {
  con: &'a Con,
  ars: &'a sml_hir::Arenas,
  atomic: bool,
  indent: usize,
}

impl fmt::Display for ConDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let needs_paren = self.atomic && self.con.arg.is_some();
    if needs_paren {
      f.write_str("(")?;
    }
    self.con.name.fmt(f)?;
    if let Some(val) = &self.con.arg {
      f.write_str(" ")?;
      ValDisplay { val: val.as_ref(), ars: self.ars, prec: Prec::Atomic, indent: self.indent }
        .fmt(f)?;
    }
    if needs_paren {
      f.write_str(")")?;
    }
    Ok(())
  }
}

struct ExceptionDisplay<'a> {
  exception: &'a Exception,
  ars: &'a sml_hir::Arenas,
  indent: usize,
}

impl fmt::Display for ExceptionDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.exception.name.fmt(f)?;
    if let Some(val) = &self.exception.arg {
      f.write_str(" ")?;
      ValDisplay { val: val.as_ref(), ars: self.ars, prec: Prec::App, indent: self.indent }
        .fmt(f)?;
    }
    Ok(())
  }
}

struct DecDisplay<'a> {
  dec: sml_hir::DecIdx,
  ars: &'a sml_hir::Arenas,
  indent: usize,
}

impl fmt::Display for DecDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.ars.dec[self.dec] {
      sml_hir::Dec::Val(_, val_binds, _) => {
        let mut iter = val_binds.iter();
        let &val_bind = iter.next().ok_or(fmt::Error)?;
        ValBindDisplay { val_bind, ars: self.ars, first: true, indent: self.indent }.fmt(f)?;
        for &val_bind in iter {
          write_nl_indent(self.indent, f)?;
          ValBindDisplay { val_bind, ars: self.ars, first: false, indent: self.indent }.fmt(f)?;
        }
        Ok(())
      }
      sml_hir::Dec::Ty(_) => f.write_str("type ..."),
      sml_hir::Dec::Datatype(_, _) | sml_hir::Dec::DatatypeCopy(_, _) => {
        f.write_str("datatype ...")
      }
      sml_hir::Dec::Abstype(_, _, _) => Err(fmt::Error),
      sml_hir::Dec::Exception(_) => f.write_str("exception ..."),
      sml_hir::Dec::Local(local_decs, in_decs) => {
        f.write_str("local")?;
        for &dec in local_decs {
          write_nl_indent(self.indent + 1, f)?;
          DecDisplay { dec, ars: self.ars, indent: self.indent + 1 }.fmt(f)?;
        }
        write_nl_indent(self.indent, f)?;
        f.write_str("in")?;
        for &dec in in_decs {
          write_nl_indent(self.indent + 1, f)?;
          DecDisplay { dec, ars: self.ars, indent: self.indent + 1 }.fmt(f)?;
        }
        write_nl_indent(self.indent, f)?;
        f.write_str("end")
      }
      sml_hir::Dec::Open(_) => f.write_str("open ..."),
    }
  }
}

struct StrDecDisplay<'a> {
  str_dec: sml_hir::StrDecIdx,
  ars: &'a sml_hir::Arenas,
  indent: usize,
}

impl fmt::Display for StrDecDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self.ars.str_dec[self.str_dec] {
      sml_hir::StrDec::Dec(decs) => {
        let iter = decs.iter().map(|&dec| DecDisplay { dec, ars: self.ars, indent: self.indent });
        nl_indent_sep(self.indent, f, iter)
      }
      sml_hir::StrDec::Structure(_) => todo!(),
      sml_hir::StrDec::Signature(_) => todo!(),
      sml_hir::StrDec::Functor(_) => todo!(),
      sml_hir::StrDec::Local(_, _) => todo!(),
    }
  }
}

struct EnvDisplay<'a> {
  env: &'a Env,
  ars: &'a sml_hir::Arenas,
  indent: usize,
}

impl fmt::Display for EnvDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (name, env) in &self.env.str {
      for _ in 0..self.indent {
        f.write_str("  ")?;
      }
      name.fmt(f)?;
      f.write_str(":\n")?;
      EnvDisplay { env, ars: self.ars, indent: self.indent + 1 }.fmt(f)?;
    }
    for (name, val) in &self.env.val {
      for _ in 0..self.indent {
        f.write_str("  ")?;
      }
      name.fmt(f)?;
      f.write_str(": ")?;
      ValDisplay { val, ars: self.ars, prec: Prec::Min, indent: self.indent }.fmt(f)?;
      f.write_str("\n")?;
    }
    Ok(())
  }
}
