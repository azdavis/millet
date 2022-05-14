use crate::util::Cx;
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, pat: Option<ast::Pat>) -> hir::PatIdx {
  todo!()
}

pub(crate) fn name(s: &str) -> hir::Pat {
  hir::Pat::Con(hir::Path::new(vec![hir::Name::new(s)]), None)
}

pub(crate) fn tuple<I>(ps: I) -> hir::Pat
where
  I: IntoIterator<Item = hir::PatIdx>,
{
  hir::Pat::Record {
    pats: ps
      .into_iter()
      .enumerate()
      .map(|(idx, p)| (hir::Lab::Num(idx + 1), p))
      .collect(),
    allows_other: false,
  }
}
