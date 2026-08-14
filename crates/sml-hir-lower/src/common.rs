//! Common utilities.

use crate::util::{Disallowed, ErrorKind, St};
use sml_syntax::ast;
use std::fmt::Write as _;

/// unfortunately, although we already kind of "parsed" these tokens in lex, that information is not
/// carried to here. so we must do it again.
///
/// TODO: make this not return option?
pub(crate) fn get_scon(st: &mut St<'_>, scon: ast::SCon) -> Option<sml_hir::SCon> {
  let tok = scon.token;
  let ret = match scon.kind {
    ast::SConKind::IntLit => {
      if !st.lang().exp.int_lit {
        st.err_tok(&tok, ErrorKind::Disallowed(Disallowed::Exp("`int` literal")));
      }
      let n = if let lex_util::num::Kind::Int { n, .. } =
        lex_util::num::get(&mut 0, tok.text().as_bytes(), |_, _| ()).kind
      {
        n
      } else {
        0
      };
      sml_hir::SCon::Int(n)
    }
    ast::SConKind::RealLit => {
      if !st.lang().exp.real_lit {
        st.err_tok(&tok, ErrorKind::Disallowed(Disallowed::Exp("`real` literal")));
      }
      let n = if let lex_util::num::Kind::Real { neg, whole, frac_leading_zeroes, frac, exp } =
        lex_util::num::get(&mut 0, tok.text().as_bytes(), |_, _| ()).kind
      {
        let mut s = String::new();
        if neg {
          s.push('-');
        }
        write!(&mut s, "{whole}").expect("write num");
        s.push('.');
        for _ in 0..frac_leading_zeroes {
          s.push('0');
        }
        write!(&mut s, "{frac}").expect("write frac");
        if exp != 0 {
          write!(&mut s, "e{exp}").expect("write exp");
        }
        match s.parse() {
          Ok(x) => x,
          Err(e) => {
            st.err_tok(&tok, ErrorKind::InvalidRealLit(e));
            0.0
          }
        }
      } else {
        0.0
      };
      sml_hir::SCon::Real(n)
    }
    ast::SConKind::WordLit => {
      if !st.lang().exp.word_lit {
        st.err_tok(&tok, ErrorKind::Disallowed(Disallowed::Exp("`word` literal")));
      }
      let n = if let lex_util::num::Kind::Word { n, .. } =
        lex_util::num::get(&mut 0, tok.text().as_bytes(), |_, _| ()).kind
      {
        n
      } else {
        0
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

pub(crate) fn get_path(p: &ast::Path) -> Option<sml_path::Path> {
  sml_path::Path::try_new(
    p.name_star_eq_dots()
      .filter_map(|x| Some(str_util::Name::new(x.name_star_eq()?.token.text())))
      .collect(),
  )
}

pub(crate) fn get_lab(st: &mut St<'_>, lab: &ast::Lab) -> sml_hir::Lab {
  match lab.kind {
    ast::LabKind::Name | ast::LabKind::Star => {
      sml_hir::Lab::Name(str_util::Name::new(lab.token.text()))
    }
    ast::LabKind::IntLit => {
      let n = if let lex_util::num::Kind::Int { n, hex } =
        lex_util::num::get(&mut 0, lab.token.text().as_bytes(), |_, _| ()).kind
      {
        if hex {
          st.err_tok(&lab.token, ErrorKind::HexNumLab);
        }
        n
      } else {
        // should never happen?
        1
      };
      let n = match usize::try_from(n) {
        Ok(x) => x,
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
  let Some(asc) = asc else { return };
  if matches!(asc.kind, ast::AscriptionKind::ColonGt) {
    st.err_tok(&asc.token, ErrorKind::InvalidOpaqueAscription);
  }
}
