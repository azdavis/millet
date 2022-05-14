use syntax::ast::{Lab, Path, SCon, SConKind};

pub(crate) fn get_scon(scon: SCon) -> Option<hir::SCon> {
  let text = scon.token.text();
  // TODO give errors?
  let ret = match scon.kind {
    SConKind::IntLit => hir::SCon::Int(text.parse().ok()?),
    SConKind::RealLit => hir::SCon::Real(text.parse().ok()?),
    SConKind::WordLit => hir::SCon::Word(text.parse().ok()?),
    SConKind::CharLit => hir::SCon::Char(text.bytes().next()?),
    SConKind::StringLit => hir::SCon::String(text.into()),
  };
  Some(ret)
}

pub(crate) fn get_path(p: Path) -> Option<hir::Path> {
  p.name_dots()
    .map(|x| x.name().map(|x| hir::Name::new(x.text())))
    .collect::<Option<_>>()
    .map(hir::Path::new)
}

pub(crate) fn get_lab(l: Lab) -> Option<hir::Lab> {
  match l {
    Lab::NameLab(l) => l.name().map(|x| hir::Lab::Name(hir::Name::new(x.text()))),
    Lab::IntLitLab(l) => l
      .int_lit()
      .and_then(|x| x.text().parse::<usize>().ok())
      .map(hir::Lab::Num),
  }
}
