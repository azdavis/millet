use crate::common::{get_lab, get_path, get_scon};
use crate::ty;
use crate::util::{Cx, ErrorKind};
use sml_syntax::ast::{self, AstNode as _, SyntaxNodePtr};

pub(crate) fn get(cx: &mut Cx, pat: Option<ast::Pat>) -> sml_hir::PatIdx {
  let pat = pat?;
  let ptr = SyntaxNodePtr::new(pat.syntax());
  let or_pat = get_or(cx, pat)?;
  if or_pat.rest.is_empty() {
    or_pat.first
  } else {
    cx.pat(sml_hir::Pat::Or(or_pat), ptr)
  }
}

fn get_or(cx: &mut Cx, pat: ast::Pat) -> Option<sml_hir::OrPat> {
  let ptr = SyntaxNodePtr::new(pat.syntax());
  let ret = match pat {
    ast::Pat::WildcardPat(_) => sml_hir::Pat::Wild,
    ast::Pat::SConPat(pat) => sml_hir::Pat::SCon(get_scon(cx, pat.s_con()?)?),
    ast::Pat::ConPat(pat) => {
      sml_hir::Pat::Con(get_path(pat.path()?)?, pat.pat().map(|x| get(cx, Some(x))))
    }
    ast::Pat::RecordPat(pat) => {
      let mut rest_pat_row = None::<RestPatRowState>;
      let rows: Vec<_> = pat
        .pat_rows()
        .filter_map(|row| match row.pat_row_inner()? {
          ast::PatRowInner::RestPatRow(_) => {
            match &mut rest_pat_row {
              None => rest_pat_row = Some(RestPatRowState::default()),
              Some(r) => r.multiple = true,
            }
            None
          }
          ast::PatRowInner::LabAndPatPatRow(row) => {
            if let Some(r) = &mut rest_pat_row {
              r.last = false;
            }
            Some((get_lab(cx, row.lab()?), get(cx, row.pat())))
          }
          ast::PatRowInner::LabPatRow(row) => {
            if let Some(r) = &mut rest_pat_row {
              r.last = false;
            }
            let lab = str_util::Name::new(row.name_star_eq()?.token.text());
            let lab_pat = name(lab.as_str());
            let ty_ann = row.ty_annotation().map(|x| ty::get(cx, x.ty()));
            let as_tail = row.as_pat_tail().map(|x| get(cx, x.pat()));
            let pat = match (ty_ann, as_tail) {
              (Some(ty), Some(pat)) => sml_hir::Pat::As(
                cx.pat(lab_pat, ptr.clone()),
                cx.pat(sml_hir::Pat::Typed(pat, ty), ptr.clone()),
              ),
              (Some(ty), None) => sml_hir::Pat::Typed(cx.pat(lab_pat, ptr.clone()), ty),
              (None, Some(pat)) => sml_hir::Pat::As(cx.pat(lab_pat, ptr.clone()), pat),
              (None, None) => lab_pat,
            };
            Some((sml_hir::Lab::Name(lab), cx.pat(pat, ptr.clone())))
          }
        })
        .collect();
      if let Some(r) = &rest_pat_row {
        if r.multiple {
          cx.err(pat.syntax().text_range(), ErrorKind::MultipleRestPatRows);
        }
        if !r.last {
          cx.err(pat.syntax().text_range(), ErrorKind::RestPatRowNotLast);
        }
      }
      sml_hir::Pat::Record { rows, allows_other: rest_pat_row.is_some() }
    }
    // sml_def(37)
    ast::Pat::ParenPat(pat) => {
      let inner = pat.pat()?;
      if warn_unnecessary_parens(&inner) {
        cx.err(pat.syntax().text_range(), ErrorKind::UnnecessaryParens);
      }
      return get_or(cx, inner);
    }
    ast::Pat::TuplePat(pat) => tuple(pat.pat_args().map(|x| get(cx, x.pat()))),
    ast::Pat::ListPat(pat) => {
      // need to rev()
      #[allow(clippy::needless_collect)]
      let pats: Vec<_> = pat.pat_args().map(|x| get(cx, x.pat())).collect();
      pats.into_iter().rev().fold(name("nil"), |ac, x| {
        let cons = sml_hir::Path::one(str_util::Name::new("::"));
        let ac = cx.pat(ac, ptr.clone());
        sml_hir::Pat::Con(cons, Some(cx.pat(tuple([x, ac]), ptr.clone())))
      })
    }
    ast::Pat::VectorPat(pat) => {
      cx.err(pat.syntax().text_range(), ErrorKind::Unsupported("vector patterns"));
      return None;
    }
    ast::Pat::InfixPat(pat) => {
      let func = sml_hir::Path::one(str_util::Name::new(pat.name_star_eq()?.token.text()));
      let lhs = get(cx, pat.lhs());
      let rhs = get(cx, pat.rhs());
      let arg = cx.pat(tuple([lhs, rhs]), ptr.clone());
      sml_hir::Pat::Con(func, Some(arg))
    }
    ast::Pat::TypedPat(pat) => {
      if pat.pat().map_or(false, has_types) {
        cx.err(pat.syntax().text_range(), ErrorKind::MultipleTypedPat);
      }
      sml_hir::Pat::Typed(get(cx, pat.pat()), ty::get(cx, pat.ty()))
    }
    ast::Pat::AsPat(pat) => {
      let lhs = get(cx, pat.pat());
      let rhs = pat.as_pat_tail()?.pat().and_then(|pat| get(cx, Some(pat)));
      sml_hir::Pat::As(lhs, rhs)
    }
    ast::Pat::OrPat(pat) => {
      // flatten or pats.
      let mut lhs = get_or(cx, pat.lhs()?)?;
      let rhs = get_or(cx, pat.rhs()?)?;
      lhs.rest.push(rhs.first);
      lhs.rest.extend(rhs.rest);
      return Some(lhs);
    }
  };
  Some(sml_hir::OrPat { first: cx.pat(ret, ptr), rest: Vec::new() })
}

fn has_types(pat: ast::Pat) -> bool {
  match pat {
    ast::Pat::WildcardPat(_) | ast::Pat::SConPat(_) => false,
    ast::Pat::ConPat(pat) => pat.pat().map_or(false, has_types),
    ast::Pat::RecordPat(pat) => pat.pat_rows().any(|pat_row| {
      pat_row.pat_row_inner().map_or(false, |inner| match inner {
        ast::PatRowInner::RestPatRow(_) => false,
        ast::PatRowInner::LabAndPatPatRow(row) => row.pat().map_or(false, has_types),
        ast::PatRowInner::LabPatRow(lab) => lab.ty_annotation().is_some(),
      })
    }),
    ast::Pat::ParenPat(pat) => pat.pat().map_or(false, has_types),
    ast::Pat::TuplePat(pat) => pat.pat_args().filter_map(|x| x.pat()).any(has_types),
    ast::Pat::ListPat(pat) => pat.pat_args().filter_map(|x| x.pat()).any(has_types),
    ast::Pat::VectorPat(pat) => {
      pat.list_pat().into_iter().flat_map(|x| x.pat_args()).filter_map(|x| x.pat()).any(has_types)
    }
    ast::Pat::InfixPat(pat) => {
      pat.lhs().map_or(false, has_types) || pat.rhs().map_or(false, has_types)
    }
    ast::Pat::TypedPat(_) => true,
    ast::Pat::AsPat(pat) => pat.as_pat_tail().and_then(|x| x.pat()).map_or(false, has_types),
    ast::Pat::OrPat(pat) => {
      pat.lhs().map_or(false, has_types) || pat.rhs().map_or(false, has_types)
    }
  }
}

/// not necessarily "is atomic".
fn warn_unnecessary_parens(pat: &ast::Pat) -> bool {
  match pat {
    ast::Pat::WildcardPat(_)
    | ast::Pat::SConPat(_)
    | ast::Pat::RecordPat(_)
    | ast::Pat::ParenPat(_)
    | ast::Pat::TuplePat(_)
    | ast::Pat::ListPat(_)
    | ast::Pat::VectorPat(_) => true,
    ast::Pat::ConPat(pat) => pat.pat().is_none(),
    ast::Pat::InfixPat(_) | ast::Pat::TypedPat(_) | ast::Pat::AsPat(_) | ast::Pat::OrPat(_) => {
      false
    }
  }
}

#[derive(Debug)]
struct RestPatRowState {
  multiple: bool,
  last: bool,
}

impl Default for RestPatRowState {
  fn default() -> Self {
    Self { multiple: false, last: true }
  }
}

pub(crate) fn name(s: &str) -> sml_hir::Pat {
  sml_hir::Pat::Con(sml_hir::Path::one(str_util::Name::new(s)), None)
}

pub(crate) fn tuple<I>(ps: I) -> sml_hir::Pat
where
  I: IntoIterator<Item = sml_hir::PatIdx>,
{
  let rows: Vec<_> =
    ps.into_iter().enumerate().map(|(idx, p)| (sml_hir::Lab::tuple(idx), p)).collect();
  assert_ne!(rows.len(), 1);
  sml_hir::Pat::Record { rows, allows_other: false }
}
