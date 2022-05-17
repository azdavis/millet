use crate::common::{get_lab, get_name, get_path, get_scon};
use crate::ty;
use crate::util::Cx;
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, pat: Option<ast::Pat>) -> hir::PatIdx {
  let pat = pat.and_then(|x| get_(cx, x)).unwrap_or(hir::Pat::None);
  cx.arenas.pat.alloc(pat)
}

fn get_(cx: &mut Cx, pat: ast::Pat) -> Option<hir::Pat> {
  let ret = match pat {
    ast::Pat::WildcardPat(_) => hir::Pat::Wild,
    ast::Pat::SConPat(pat) => hir::Pat::SCon(get_scon(pat.s_con()?)?),
    ast::Pat::ConPat(pat) => {
      hir::Pat::Con(get_path(pat.path()?)?, pat.pat().map(|x| get(cx, Some(x))))
    }
    ast::Pat::RecordPat(pat) => {
      let mut allows_other = false;
      let rows: Vec<_> = pat
        .pat_rows()
        .filter_map(|row| match row.pat_row_inner()? {
          ast::PatRowInner::RestPatRow(_) => {
            allows_other = true;
            None
          }
          ast::PatRowInner::LabAndPatPatRow(row) => {
            Some((get_lab(row.lab()?)?, get(cx, row.pat())))
          }
          ast::PatRowInner::LabPatRow(row) => {
            let name = get_name(row.name())?;
            let pat = as_(cx, name.clone(), row.ty_annotation(), row.as_pat_tail()?);
            Some((hir::Lab::Name(name), cx.arenas.pat.alloc(pat)))
          }
        })
        .collect();
      hir::Pat::Record { rows, allows_other }
    }
    ast::Pat::TuplePat(pat) => tuple(pat.pat_args().map(|x| get(cx, x.pat()))),
    ast::Pat::ListPat(pat) => {
      // need to rev()
      #[allow(clippy::needless_collect)]
      let pats: Vec<_> = pat.pat_args().map(|x| get(cx, x.pat())).collect();
      pats.into_iter().rev().fold(name("nil"), |ac, x| {
        let cons = hir::Path::one(hir::Name::new("::"));
        let ac = cx.arenas.pat.alloc(ac);
        hir::Pat::Con(cons, Some(cx.arenas.pat.alloc(tuple([x, ac]))))
      })
    }
    ast::Pat::InfixPat(pat) => {
      let func = hir::Path::one(get_name(pat.name())?);
      let lhs = get(cx, pat.lhs());
      let rhs = get(cx, pat.rhs());
      let arg = cx.arenas.pat.alloc(tuple([lhs, rhs]));
      hir::Pat::Con(func, Some(arg))
    }
    ast::Pat::TypedPat(pat) => hir::Pat::Typed(
      get(cx, pat.pat()),
      ty::get(cx, pat.ty_annotation().and_then(|x| x.ty())),
    ),
    ast::Pat::AsPat(pat) => as_(
      cx,
      get_name(pat.name())?,
      pat.ty_annotation(),
      pat.as_pat_tail()?,
    ),
  };
  Some(ret)
}

pub(crate) fn name(s: &str) -> hir::Pat {
  hir::Pat::Con(hir::Path::one(hir::Name::new(s)), None)
}

/// TODO do not make 1-tuples
pub(crate) fn tuple<I>(ps: I) -> hir::Pat
where
  I: IntoIterator<Item = hir::PatIdx>,
{
  hir::Pat::Record {
    rows: ps
      .into_iter()
      .enumerate()
      .map(|(idx, p)| (hir::Lab::Num(idx + 1), p))
      .collect(),
    allows_other: false,
  }
}

fn as_(
  cx: &mut Cx,
  name: hir::Name,
  annot: Option<ast::TyAnnotation>,
  tail: ast::AsPatTail,
) -> hir::Pat {
  let ty = annot.map(|x| ty::get(cx, x.ty()));
  let mut inner = get(cx, tail.pat());
  if let Some(ty) = ty {
    inner = cx.arenas.pat.alloc(hir::Pat::Typed(inner, ty));
  }
  hir::Pat::As(name, inner)
}
