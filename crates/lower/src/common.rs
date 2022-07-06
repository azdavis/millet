use crate::util::{Cx, ErrorKind};
use syntax::ast;

/// unfortunately, although we already kind of "parsed" these tokens in lex, that information is not
/// carried to here. so we must do it again.
pub(crate) fn get_scon(cx: &mut Cx, scon: ast::SCon) -> Option<hir::SCon> {
  let tok = scon.token;
  let ret = match scon.kind {
    ast::SConKind::IntLit => {
      let chars = tok.text();
      let (mul, mut chars) = start_int(chars);
      let radix = if chars.as_str().starts_with("0x") {
        chars.next();
        chars.next();
        16
      } else {
        10
      };
      let n = match i32::from_str_radix(chars.as_str(), radix) {
        Ok(x) => mul * x,
        Err(e) => {
          cx.err(tok.text_range(), ErrorKind::InvalidIntLit(e));
          0
        }
      };
      hir::SCon::Int(n)
    }
    ast::SConKind::RealLit => {
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
          cx.err(tok.text_range(), ErrorKind::InvalidRealLit(e));
          0.0
        }
      };
      hir::SCon::Real(n)
    }
    ast::SConKind::WordLit => {
      let mut chars = tok.text().chars();
      // 0
      chars.next();
      // w
      chars.next();
      let radix = if chars.as_str().starts_with('x') {
        chars.next();
        16
      } else {
        10
      };
      let n = match u32::from_str_radix(chars.as_str(), radix) {
        Ok(x) => x,
        Err(e) => {
          cx.err(tok.text_range(), ErrorKind::InvalidIntLit(e));
          0
        }
      };
      hir::SCon::Word(n)
    }
    ast::SConKind::CharLit => hir::SCon::Char(
      tok
        .text()
        .strip_prefix("#\"")?
        .strip_suffix('"')?
        .chars()
        .next()?,
    ),
    ast::SConKind::StringLit => {
      hir::SCon::String(tok.text().strip_prefix('"')?.strip_suffix('"')?.into())
    }
  };
  Some(ret)
}

fn start_int(chars: &str) -> (i32, std::str::Chars<'_>) {
  let mut chars = chars.chars();
  let neg = chars.as_str().starts_with('~');
  if neg {
    chars.next();
  }
  (if neg { -1 } else { 1 }, chars)
}

pub(crate) fn get_name(n: Option<syntax::SyntaxToken>) -> Option<hir::Name> {
  n.map(|tok| hir::Name::new(tok.text()))
}

pub(crate) fn get_path(p: ast::Path) -> Option<hir::Path> {
  hir::Path::try_new(
    p.name_star_eq_dots()
      .filter_map(|x| Some(hir::Name::new(x.name_star_eq()?.token.text())))
      .collect(),
  )
}

pub(crate) fn get_lab(cx: &mut Cx, lab: ast::Lab) -> hir::Lab {
  match lab.kind {
    ast::LabKind::Name | ast::LabKind::Star => hir::Lab::Name(hir::Name::new(lab.token.text())),
    ast::LabKind::IntLit => {
      let n = match lab.token.text().parse::<usize>() {
        Ok(n) => n,
        Err(e) => {
          cx.err(lab.token.text_range(), ErrorKind::InvalidNumLab(e));
          1
        }
      };
      if n == 0 {
        cx.err(lab.token.text_range(), ErrorKind::ZeroNumLab);
      }
      hir::Lab::Num(n)
    }
  }
}
