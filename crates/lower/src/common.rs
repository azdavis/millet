use syntax::ast;

pub(crate) fn get_scon(scon: ast::SCon) -> Option<hir::SCon> {
  let text = scon.token.text();
  // TODO give errors?
  let ret = match scon.kind {
    ast::SConKind::IntLit => hir::SCon::Int(text.parse().ok()?),
    ast::SConKind::RealLit => hir::SCon::Real(text.parse().ok()?),
    ast::SConKind::WordLit => hir::SCon::Word(text.parse().ok()?),
    ast::SConKind::CharLit => hir::SCon::Char(text.bytes().next()?),
    ast::SConKind::StringLit => hir::SCon::String(text.into()),
  };
  Some(ret)
}

pub(crate) fn get_name(n: Option<syntax::SyntaxToken>) -> Option<hir::Name> {
  n.map(|x| hir::Name::new(x.text()))
}

pub(crate) fn get_path(p: ast::Path) -> Option<hir::Path> {
  p.name_dots()
    .map(|x| get_name(x.name()))
    .collect::<Option<_>>()
    .map(hir::Path::new)
}

pub(crate) fn get_lab(l: ast::Lab) -> Option<hir::Lab> {
  match l {
    ast::Lab::NameLab(l) => get_name(l.name()).map(hir::Lab::Name),
    ast::Lab::IntLitLab(l) => l
      .int_lit()
      .and_then(|x| x.text().parse::<usize>().ok())
      .map(hir::Lab::Num),
  }
}
