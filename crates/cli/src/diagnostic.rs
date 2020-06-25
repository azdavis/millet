//! Conversion from library error types to codespan Diagnostics.

use crate::source::SourceId;
use codespan_reporting::diagnostic::Label;
use millet_core::ast::Label as AstLabel;
use millet_core::intern::StrStore;
use millet_core::lex::LexError;
use millet_core::loc::{Loc, Located};
use millet_core::parse::ParseError;
use millet_core::statics::{StaticsError, Ty};

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<SourceId>;

pub fn io(name: &str, err: std::io::Error) -> Diagnostic {
  Diagnostic::error().with_message(format!("{}: {}", name, err))
}

pub fn lex(id: SourceId, err: Located<LexError>) -> Diagnostic {
  let msg = match err.val {
    LexError::UnmatchedCloseComment => "unmatched close comment".to_owned(),
    LexError::UnmatchedOpenComment => "unmatched open comment".to_owned(),
    LexError::IncompleteTypeVar => "incomplete type var".to_owned(),
    LexError::UnknownByte(b) => format!("unknown byte: {}", b),
    LexError::InvalidIntConstant(e) => format!("invalid integer constant: {}", e),
    LexError::InvalidRealConstant(e) => format!("invalid real constant: {}", e),
    LexError::NegativeWordConstant => "negative word constant".to_owned(),
    LexError::IncompleteNumConstant => "incomplete numeric constant".to_owned(),
    LexError::UnclosedStringConstant => "unclosed string constant".to_owned(),
    LexError::InvalidStringConstant => "invalid string constant".to_owned(),
    LexError::InvalidCharConstant => "invalid character constant".to_owned(),
  };
  simple(msg, id, err.loc)
}

pub fn parse(store: &StrStore, id: SourceId, err: Located<ParseError>) -> Diagnostic {
  let msg = match err.val {
    ParseError::ExpectedButFound(exp, fnd) => format!("expected {}, found {}", exp, fnd),
    ParseError::InfixWithoutOp(id) => format!(
      "infix identifier used without preceding `op`: {}",
      store.get(id)
    ),
    ParseError::NotInfix(id) => format!("non-infix identifier used as infix: {}", store.get(id)),
    ParseError::RealPat => "real constant used as a pattern".to_owned(),
    ParseError::NegativeFixity(n) => format!("fixity is negative: {}", n),
    ParseError::SameFixityDiffAssoc => {
      "consecutive infix identifiers with same fixity but different associativity".to_owned()
    }
  };
  simple(msg, id, err.loc)
}

pub fn statics(store: &StrStore, id: SourceId, err: StaticsError) -> Diagnostic {
  match err {
    StaticsError::Undefined(item, name) => {
      let msg = format!("undefined {} identifier: {}", item, store.get(name.val));
      simple(msg, id, name.loc)
    }
    StaticsError::Redefined(name) => {
      let msg = format!("redefined identifier: {}", store.get(name.val));
      simple(msg, id, name.loc)
    }
    StaticsError::DuplicateLabel(lab) => {
      let msg = format!("duplicate label: {}", show_lab(store, lab.val));
      simple(msg, id, lab.loc)
    }
    StaticsError::Circularity(loc, ty_var, ty) => {
      let msg = format!("circularity: {} in {}", ty_var, show_ty(store, &ty));
      simple(msg, id, loc)
    }
    StaticsError::HeadMismatch(loc, lhs, rhs) => {
      let msg = format!(
        "mismatched types: {} vs {}",
        show_ty(store, &lhs),
        show_ty(store, &rhs)
      );
      simple(msg, id, loc)
    }
    StaticsError::MissingLabel(loc, lab) => {
      let msg = format!("type is missing label {}", show_lab(store, lab));
      simple(msg, id, loc)
    }
    StaticsError::ValAsPat(loc) => simple("value binding used as pattern", id, loc),
    StaticsError::WrongNumTyArgs(loc, want, got) => {
      let msg = format!(
        "wrong number of type arguments: expected {}, found {}",
        want, got
      );
      simple(msg, id, loc)
    }
    StaticsError::NonVarInAs(name) => {
      let msg = format!(
        "pattern to left of `as` is not a variable: {}",
        store.get(name.val)
      );
      simple(msg, id, name.loc)
    }
    StaticsError::ForbiddenBinding(loc, name) => {
      let msg = format!("forbidden identifier in binding: {}", store.get(name));
      simple(msg, id, loc)
    }
    StaticsError::NoSuitableOverload(loc) => simple("no suitable overload found", id, loc),
    StaticsError::TyNameEscape(loc) => {
      simple("expression causes a type name to escape its scope", id, loc)
    }
    StaticsError::Todo(loc) => simple("unimplemented language construct", id, loc),
  }
}

fn simple<M: Into<String>>(msg: M, id: SourceId, loc: Loc) -> Diagnostic {
  Diagnostic::error()
    .with_message(msg)
    .with_labels(vec![Label::primary(id, loc)])
}

fn show_lab(store: &StrStore, lab: AstLabel) -> String {
  match lab {
    AstLabel::Vid(id) => store.get(id).to_owned(),
    AstLabel::Num(n) => format!("{}", n),
  }
}

fn show_ty(store: &StrStore, ty: &Ty) -> String {
  let mut buf = String::new();
  show_ty_impl(&mut buf, store, ty);
  buf
}

fn show_ty_impl(buf: &mut String, store: &StrStore, ty: &Ty) {
  match ty {
    Ty::Var(tv) => buf.push_str(&tv.to_string()),
    Ty::Record(rows) => {
      buf.push_str("{ ");
      let mut rows = rows.iter();
      if let Some((lab, ty)) = rows.next() {
        show_row(buf, store, *lab, ty);
      }
      for (lab, ty) in rows {
        buf.push_str(", ");
        show_row(buf, store, *lab, ty);
      }
      buf.push_str(" }");
    }
    Ty::Arrow(lhs, rhs) => {
      buf.push_str("(");
      show_ty_impl(buf, store, lhs);
      buf.push_str(") -> (");
      show_ty_impl(buf, store, rhs);
      buf.push_str(")");
    }
    Ty::Ctor(args, sym) => {
      if args.is_empty() {
        buf.push_str(store.get(sym.name()));
        return;
      }
      buf.push_str("(");
      let mut args = args.iter();
      if let Some(arg) = args.next() {
        show_ty_impl(buf, store, arg);
      }
      for arg in args {
        buf.push_str(", ");
        show_ty_impl(buf, store, arg);
      }
      buf.push_str(") ");
      buf.push_str(store.get(sym.name()));
    }
  }
}

fn show_row(buf: &mut String, store: &StrStore, lab: AstLabel, ty: &Ty) {
  buf.push_str(&show_lab(store, lab));
  buf.push_str(" : ");
  show_ty_impl(buf, store, ty);
}
