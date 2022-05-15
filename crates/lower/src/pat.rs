use crate::util::Cx;
use syntax::ast;

pub(crate) fn get(cx: &mut Cx, pat: Option<ast::Pat>) -> hir::PatIdx {
  let pat = pat.and_then(|x| get_(cx, x)).unwrap_or(hir::Pat::None);
  cx.arenas.pat.alloc(pat)
}

fn get_(_: &mut Cx, pat: ast::Pat) -> Option<hir::Pat> {
  let ret = match pat {
    ast::Pat::WildcardPat(_) => hir::Pat::Wild,
    ast::Pat::SConPat(_) => todo!(),
    ast::Pat::ConPat(_) => todo!(),
    ast::Pat::RecordPat(_) => todo!(),
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
