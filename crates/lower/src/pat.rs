use crate::common::{get_lab, get_path, get_scon};
use crate::ty;
use crate::util::{Cx, ErrorKind};
use syntax::ast::{self, AstNode as _, AstPtr};

pub(crate) fn get(cx: &mut Cx, pat: Option<ast::Pat>) -> hir::PatIdx {
  let pat = pat?;
  let ptr = AstPtr::new(&pat);
  let ret = match pat {
    ast::Pat::WildcardPat(_) => hir::Pat::Wild,
    ast::Pat::SConPat(pat) => hir::Pat::SCon(get_scon(cx, pat.s_con()?)?),
    ast::Pat::ConPat(pat) => {
      hir::Pat::Con(get_path(pat.path()?)?, pat.pat().map(|x| get(cx, Some(x))))
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
            let lab = hir::Name::new(row.name_star_eq()?.token.text());
            let ty_ann = row.ty_annotation().map(|x| ty::get(cx, x.ty()));
            let as_tail = row.as_pat_tail().map(|x| get(cx, x.pat()));
            let pat = match (ty_ann, as_tail) {
              (Some(ty), Some(pat)) => {
                hir::Pat::As(lab.clone(), cx.pat(hir::Pat::Typed(pat, ty), ptr.clone()))
              }
              (Some(ty), None) => hir::Pat::Typed(cx.pat(name(lab.as_str()), ptr.clone()), ty),
              (None, Some(pat)) => hir::Pat::As(lab.clone(), pat),
              (None, None) => name(lab.as_str()),
            };
            Some((hir::Lab::Name(lab), cx.pat(pat, ptr.clone())))
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
      hir::Pat::Record {
        rows,
        allows_other: rest_pat_row.is_some(),
      }
    }
    // sml_def(37)
    ast::Pat::ParenPat(pat) => return get(cx, pat.pat()),
    ast::Pat::TuplePat(pat) => tuple(pat.pat_args().map(|x| get(cx, x.pat()))),
    ast::Pat::ListPat(pat) => {
      // need to rev()
      #[allow(clippy::needless_collect)]
      let pats: Vec<_> = pat.pat_args().map(|x| get(cx, x.pat())).collect();
      pats.into_iter().rev().fold(name("nil"), |ac, x| {
        let cons = hir::Path::one(hir::Name::new("::"));
        let ac = cx.pat(ac, ptr.clone());
        hir::Pat::Con(cons, Some(cx.pat(tuple([x, ac]), ptr.clone())))
      })
    }
    ast::Pat::VectorPat(pat) => {
      cx.err(
        pat.syntax().text_range(),
        ErrorKind::Unsupported("vector patterns"),
      );
      return None;
    }
    ast::Pat::InfixPat(pat) => {
      let func = hir::Path::one(hir::Name::new(pat.name_star_eq()?.token.text()));
      let lhs = get(cx, pat.lhs());
      let rhs = get(cx, pat.rhs());
      let arg = cx.pat(tuple([lhs, rhs]), ptr.clone());
      hir::Pat::Con(func, Some(arg))
    }
    ast::Pat::TypedPat(pat) => hir::Pat::Typed(
      get(cx, pat.pat()),
      ty::get(cx, pat.ty_annotation().and_then(|x| x.ty())),
    ),
    ast::Pat::TypedNamePat(pat) => {
      let name_pat = cx.pat(name(pat.name_star_eq()?.token.text()), ptr.clone());
      hir::Pat::Typed(
        name_pat,
        ty::get(cx, pat.ty_annotation().and_then(|x| x.ty())),
      )
    }
    ast::Pat::OrPat(pat) => {
      cx.err(
        pat.syntax().text_range(),
        ErrorKind::Unsupported("or patterns"),
      );
      return None;
    }
    ast::Pat::AsPat(pat) => {
      let name = hir::Name::new(pat.name_star_eq()?.token.text());
      let ty = pat.ty_annotation().map(|x| ty::get(cx, x.ty()));
      let inner = pat.as_pat_tail()?.pat().and_then(|pat| {
        let ptr = AstPtr::new(&pat);
        let mut p = get(cx, Some(pat));
        if let Some(ty) = ty {
          p = cx.pat(hir::Pat::Typed(p, ty), ptr);
        }
        p
      });
      hir::Pat::As(name, inner)
    }
  };
  cx.pat(ret, ptr)
}

#[derive(Debug)]
struct RestPatRowState {
  multiple: bool,
  last: bool,
}

impl Default for RestPatRowState {
  fn default() -> Self {
    Self {
      multiple: false,
      last: true,
    }
  }
}

pub(crate) fn name(s: &str) -> hir::Pat {
  hir::Pat::Con(hir::Path::one(hir::Name::new(s)), None)
}

pub(crate) fn tuple<I>(ps: I) -> hir::Pat
where
  I: IntoIterator<Item = hir::PatIdx>,
{
  let rows: Vec<_> = ps
    .into_iter()
    .enumerate()
    .map(|(idx, p)| (hir::Lab::tuple(idx), p))
    .collect();
  assert_ne!(rows.len(), 1);
  hir::Pat::Record {
    rows,
    allows_other: false,
  }
}
