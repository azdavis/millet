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
      let path = get_path(pat.path()?)?;
      let arg = pat.pat().map(|x| get(cx, Some(x)));
      hir::Pat::Con(path, arg)
    }
    ast::Pat::RecordPat(pat) => {
      let mut allows_other = false;
      let rows: Vec<_> = pat
        .pat_rows()
        .filter_map(|x| match x.pat_row_inner()? {
          ast::PatRowInner::RestPatRow(_) => {
            allows_other = true;
            None
          }
          ast::PatRowInner::LabAndPatPatRow(row) => {
            Some((get_lab(row.lab()?)?, get(cx, row.pat())))
          }
          ast::PatRowInner::LabPatRow(row) => {
            let name = get_name(row.name())?;
            let ty = row.ty_annotation().map(|x| ty::get(cx, x.ty()));
            let mut inner = get(cx, row.as_pat_tail()?.pat());
            if let Some(ty) = ty {
              inner = cx.arenas.pat.alloc(hir::Pat::Typed(inner, ty));
            }
            let pat = cx.arenas.pat.alloc(hir::Pat::As(name.clone(), inner));
            Some((hir::Lab::Name(name), pat))
          }
        })
        .collect();
      hir::Pat::Record { rows, allows_other }
    }
    ast::Pat::TuplePat(_) => todo!(),
    ast::Pat::ListPat(_) => todo!(),
    ast::Pat::InfixPat(_) => todo!(),
    ast::Pat::TypedPat(_) => todo!(),
    ast::Pat::AsPat(_) => todo!(),
  };
  Some(ret)
}

pub(crate) fn name(s: &str) -> hir::Pat {
  hir::Pat::Con(hir::Path::one(hir::Name::new(s)), None)
}

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
