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
  n.map(|tok| hir::Name::new(tok.text()))
}

pub(crate) fn get_path(p: ast::Path) -> Option<hir::Path> {
  hir::Path::try_new(p.name_dots().filter_map(|x| get_name(x.name())).collect())
}

pub(crate) fn get_lab(lab: ast::Lab) -> Option<hir::Lab> {
  match lab.kind {
    ast::LabKind::Name => Some(hir::Lab::Name(hir::Name::new(lab.token.text()))),
    ast::LabKind::IntLit => lab.token.text().parse::<usize>().ok().map(hir::Lab::Num),
  }
}
