//! Common utilities.

#![allow(clippy::needless_pass_by_value)]

use crate::util::{Disallowed, ErrorKind, St};
use sml_syntax::ast;

/// unfortunately, although we already kind of "parsed" these tokens in lex, that information is not
/// carried to here. so we must do it again.
pub(crate) fn get_scon(st: &mut St<'_>, scon: ast::SCon) -> Option<sml_hir::SCon> {
  let tok = scon.token;
  let ret = match scon.kind {
    ast::SConKind::IntLit => {
      if !st.lang().exp.int_lit {
        st.err_tok(&tok, ErrorKind::Disallowed(Disallowed::Exp("`int` literal")));
      }
      let chars = tok.text();
      let mut chars = chars.chars();
      let neg = chars.as_str().starts_with('~');
      if neg {
        chars.next();
      }
      let mul: i32 = if neg { -1 } else { 1 };
      let radix: u32 = if chars.as_str().starts_with("0x") {
        chars.next();
        chars.next();
        16
      } else {
        10
      };
      let n = match sml_hir::Int::from_str_radix(chars.as_str(), radix) {
        Ok(x) => x * mul,
        Err(e) => {
          st.err_tok(&tok, ErrorKind::InvalidIntLit(e));
          sml_hir::Int::from(0i32)
        }
      };
      sml_hir::SCon::Int(n)
    }
    ast::SConKind::RealLit => {
      if !st.lang().exp.real_lit {
        st.err_tok(&tok, ErrorKind::Disallowed(Disallowed::Exp("`real` literal")));
      }
      let owned: String;
      let mut text = tok.text();
      // only alloc if needed
      if text.contains('~') {
        owned = tok.text().replace('~', "-");
        text = owned.as_str();
      }
      let n = match text.parse() {
        Ok(x) => x,
        Err(e) => {
          st.err_tok(&tok, ErrorKind::InvalidRealLit(e));
          0.0
        }
      };
      sml_hir::SCon::Real(n)
    }
    ast::SConKind::WordLit => {
      if !st.lang().exp.word_lit {
        st.err_tok(&tok, ErrorKind::Disallowed(Disallowed::Exp("`word` literal")));
      }
      let mut chars = tok.text().chars();
      // 0
      chars.next();
      // w
      chars.next();
      let radix: u32 = if chars.as_str().starts_with('x') {
        chars.next();
        16
      } else {
        10
      };
      let n = match u64::from_str_radix(chars.as_str(), radix) {
        Ok(x) => x,
        Err(e) => {
          st.err_tok(&tok, ErrorKind::InvalidWordLit(e));
          0
        }
      };
      sml_hir::SCon::Word(n)
    }
    ast::SConKind::CharLit => {
      if !st.lang().exp.char_lit {
        st.err_tok(&tok, ErrorKind::Disallowed(Disallowed::Exp("`char` literal")));
      }
      sml_hir::SCon::Char(sml_string(tok.text().strip_prefix('#')?)?.chars().next()?)
    }
    ast::SConKind::StringLit => {
      if !st.lang().exp.string_lit {
        st.err_tok(&tok, ErrorKind::Disallowed(Disallowed::Exp("`string` literal")));
      }
      sml_hir::SCon::String(sml_string(tok.text())?.into())
    }
  };
  Some(ret)
}

fn sml_string(s: &str) -> Option<String> {
  let mut idx = 0usize;
  let res = lex_util::string::get(&mut idx, s.as_bytes());
  if idx == s.len() { res.actual } else { None }
}

pub(crate) fn get_name(n: Option<sml_syntax::kind::SyntaxToken>) -> Option<str_util::Name> {
  n.map(|tok| str_util::Name::new(tok.text()))
}

pub(crate) fn get_path(p: ast::Path) -> Option<sml_path::Path> {
  sml_path::Path::try_new(
    p.name_star_eq_dots()
      .filter_map(|x| Some(str_util::Name::new(x.name_star_eq()?.token.text())))
      .collect(),
  )
}

pub(crate) fn get_lab(st: &mut St<'_>, lab: ast::Lab) -> sml_hir::Lab {
  match lab.kind {
    ast::LabKind::Name | ast::LabKind::Star => {
      sml_hir::Lab::Name(str_util::Name::new(lab.token.text()))
    }
    ast::LabKind::IntLit => {
      let n = match lab.token.text().parse::<usize>() {
        Ok(n) => n,
        Err(e) => {
          st.err_tok(&lab.token, ErrorKind::InvalidNumLab(e));
          1
        }
      };
      if n == 0 {
        st.err_tok(&lab.token, ErrorKind::ZeroNumLab);
      }
      sml_hir::Lab::Num(n)
    }
  }
}

pub(crate) fn forbid_opaque_asc(st: &mut St<'_>, asc: Option<ast::Ascription>) {
  let asc = match asc {
    None => return,
    Some(x) => x,
  };
  if matches!(asc.kind, ast::AscriptionKind::ColonGt) {
    st.err_tok(&asc.token, ErrorKind::InvalidOpaqueAscription);
  }
}
