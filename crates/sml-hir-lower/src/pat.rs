//! Lowering patterns.

use crate::common::{forbid_opaque_asc, get_lab, get_path, get_scon};
use crate::ty;
use crate::util::{Disallowed, ErrorKind, MatcherFlavor, Sep, St};
use sml_syntax::ast::{self, AstNode as _, SyntaxNodePtr};

pub(crate) fn get(
  st: &mut St<'_>,
  flavor: Option<MatcherFlavor>,
  pat: Option<ast::Pat>,
) -> sml_hir::PatIdx {
  let pat = pat?;
  let ptr = SyntaxNodePtr::new(pat.syntax());
  let or_pat = get_or(st, flavor, pat)?;
  if or_pat.rest.is_empty() {
    or_pat.first
  } else {
    st.pat(sml_hir::Pat::Or(or_pat), ptr)
  }
}

fn get_or(st: &mut St<'_>, flavor: Option<MatcherFlavor>, pat: ast::Pat) -> Option<sml_hir::OrPat> {
  let ptr = SyntaxNodePtr::new(pat.syntax());
  let ret = match pat {
    ast::Pat::WildcardPat(_) => sml_hir::Pat::Wild,
    ast::Pat::SConPat(pat) => sml_hir::Pat::SCon(get_scon(st, pat.s_con()?)?),
    ast::Pat::ConPat(pat) => {
      if let Some(flavor) = flavor {
        let mut iter = pat.path()?.name_star_eq_dots();
        let name = iter.next()?.name_star_eq()?.token;
        if iter.next().is_none() && st.is_name_of_cur_fun(name.text()) {
          st.err_tok(&name, ErrorKind::PatNameIsNameOfContainingFun(flavor));
        }
      }
      sml_hir::Pat::Con(get_path(pat.path()?)?, pat.pat().map(|x| get(st, flavor, Some(x))))
    }
    ast::Pat::RecordPat(pat) => {
      if let Some(s) = pat.pat_rows().last().and_then(|x| x.comma()) {
        st.err_tok(&s, ErrorKind::Trailing(Sep::Comma));
      }
      let mut rest_pat_row = None::<RestPatRowState>;
      let rows = pat.pat_rows().filter_map(|row| match row.pat_row_inner()? {
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
          Some((get_lab(st, row.lab()?), get(st, flavor, row.pat())))
        }
        ast::PatRowInner::LabPatRow(row) => {
          if let Some(r) = &mut rest_pat_row {
            r.last = false;
          }
          let lab = str_util::Name::new(row.name_star_eq()?.token.text());
          let ty_ann = row.ty_annotation().map(|ty_ann| {
            forbid_opaque_asc(st, ty_ann.ascription());
            ty::get(st, ty_ann.ty())
          });
          let as_tail = row.as_pat_tail().map(|x| get(st, flavor, x.pat()));
          let pat = match (ty_ann, as_tail) {
            (Some(ty), Some(pat)) => {
              sml_hir::Pat::As(lab.clone(), st.pat(sml_hir::Pat::Typed(pat, ty), ptr.clone()))
            }
            (Some(ty), None) => sml_hir::Pat::Typed(st.pat(name(lab.as_str()), ptr.clone()), ty),
            (None, Some(pat)) => sml_hir::Pat::As(lab.clone(), pat),
            (None, None) => name(lab.as_str()),
          };
          Some((sml_hir::Lab::Name(lab), st.pat(pat, ptr.clone())))
        }
      });
      let rows: Vec<_> = rows.collect();
      if rows.is_empty() {
        st.err(pat.syntax(), ErrorKind::EmptyRecordPatOrExp);
      }
      if let Some(r) = &rest_pat_row {
        if r.multiple {
          st.err(pat.syntax(), ErrorKind::MultipleRestPatRows);
        }
        if !r.last {
          st.err(pat.syntax(), ErrorKind::RestPatRowNotLast);
        }
      }
      sml_hir::Pat::Record { rows, allows_other: rest_pat_row.is_some() }
    }
    // @def(37)
    ast::Pat::ParenPat(pat) => {
      let inner = pat.pat()?;
      if warn_unnecessary_parens(&inner) {
        st.err(pat.syntax(), ErrorKind::UnnecessaryParens);
      }
      return get_or(st, flavor, inner);
    }
    ast::Pat::TuplePat(pat) => {
      if let Some(s) = pat.pat_args().last().and_then(|x| x.comma()) {
        st.err_tok(&s, ErrorKind::Trailing(Sep::Comma));
      }
      tuple(pat.pat_args().map(|x| get(st, flavor, x.pat())))
    }
    ast::Pat::ListPat(pat) => {
      if let Some(s) = pat.pat_args().last().and_then(|x| x.comma()) {
        st.err_tok(&s, ErrorKind::Trailing(Sep::Comma));
      }
      // need to rev()
      #[allow(clippy::needless_collect)]
      let pats: Vec<_> = pat.pat_args().map(|x| get(st, flavor, x.pat())).collect();
      pats.into_iter().rev().fold(name("nil"), |ac, x| {
        let cons = sml_path::Path::one(str_util::Name::new("::"));
        let ac = st.pat(ac, ptr.clone());
        sml_hir::Pat::Con(cons, Some(st.pat(tuple([x, ac]), ptr.clone())))
      })
    }
    ast::Pat::VectorPat(pat) => {
      if !st.lang().successor_ml.vector {
        st.err(pat.syntax(), ErrorKind::Disallowed(Disallowed::SuccMl("vector patterns")));
      }
      let last_comma =
        pat.list_pat().into_iter().flat_map(|x| x.pat_args()).last().and_then(|x| x.comma());
      if let Some(s) = last_comma {
        st.err_tok(&s, ErrorKind::Trailing(Sep::Comma));
      }
      let pats: Vec<_> = pat
        .list_pat()
        .into_iter()
        .flat_map(|x| x.pat_args())
        .map(|x| get(st, flavor, x.pat()))
        .collect();
      sml_hir::Pat::Vector(pats)
    }
    ast::Pat::InfixPat(pat) => {
      let func = sml_path::Path::one(str_util::Name::new(pat.name_star_eq()?.token.text()));
      let lhs = get(st, flavor, pat.lhs());
      let rhs = get(st, flavor, pat.rhs());
      let arg = st.pat(tuple([lhs, rhs]), ptr.clone());
      sml_hir::Pat::Con(func, Some(arg))
    }
    ast::Pat::TypedPat(pat) => {
      if pat.pat().map_or(false, has_types) {
        st.err(pat.syntax(), ErrorKind::MultipleTypedPat);
      }
      forbid_opaque_asc(st, pat.ascription());
      sml_hir::Pat::Typed(get(st, flavor, pat.pat()), ty::get(st, pat.ty()))
    }
    ast::Pat::AsPat(pat) => {
      let mut rhs = pat.as_pat_tail()?.pat().and_then(|pat| get(st, flavor, Some(pat)));
      let lhs = pat.pat()?;
      let name = match lhs {
        ast::Pat::TypedPat(pat) => {
          let ty = ty::get(st, pat.ty());
          rhs = st.pat(sml_hir::Pat::Typed(rhs, ty), ptr.clone());
          get_pat_name(st, pat.pat()?)?
        }
        pat => get_pat_name(st, pat)?,
      };
      sml_hir::Pat::As(name, rhs)
    }
    ast::Pat::OrPat(pat) => {
      if !st.lang().successor_ml.or_pat {
        st.err(pat.syntax(), ErrorKind::Disallowed(Disallowed::SuccMl("or patterns")));
      }
      // flatten or pats.
      let mut lhs = get_or(st, flavor, pat.lhs()?)?;
      let rhs = get_or(st, flavor, pat.rhs()?)?;
      lhs.rest.push(rhs.first);
      lhs.rest.extend(rhs.rest);
      return Some(lhs);
    }
  };
  Some(sml_hir::OrPat { first: st.pat(ret, ptr), rest: Vec::new() })
}

fn get_pat_name(st: &mut St<'_>, pat: ast::Pat) -> Option<str_util::Name> {
  let ast::Pat::ConPat(pat) = pat else {
    st.err(pat.syntax(), ErrorKind::AsPatLhsNotName);
    return None;
  };
  if pat.pat().is_some() {
    st.err(pat.syntax(), ErrorKind::AsPatLhsNotName);
    return None;
  }
  let mut iter = pat.path()?.name_star_eq_dots();
  let name = str_util::Name::new(iter.next()?.name_star_eq()?.token.text());
  if iter.next().is_some() {
    st.err(pat.syntax(), ErrorKind::AsPatLhsNotName);
    return None;
  }
  Some(name)
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
    ast::Pat::ConPat(pat) => pat.pat().is_none() && pat.op_kw().is_none(),
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
  sml_hir::Pat::Con(sml_path::Path::one(str_util::Name::new(s)), None)
}

pub(crate) fn tuple<I>(ps: I) -> sml_hir::Pat
where
  I: IntoIterator<Item = sml_hir::PatIdx>,
{
  let rows: Vec<_> =
    ps.into_iter().enumerate().map(|(idx, p)| (sml_hir::Lab::tuple(idx), p)).collect();
  // can't assert rows.len() != 1 because of this
  cov_mark::hit("trailing_pat_arg");
  sml_hir::Pat::Record { rows, allows_other: false }
}
