use syntax::ast::{Lab, Path, SConKind};
use syntax::SyntaxToken;

pub(crate) fn scon(kind: SConKind) -> hir::SCon {
  match kind {
    SConKind::IntLit => hir::SCon::Int,
    SConKind::RealLit => hir::SCon::Real,
    SConKind::WordLit => hir::SCon::Word,
    SConKind::CharLit => hir::SCon::Char,
    SConKind::StringLit => hir::SCon::String,
  }
}

pub(crate) fn path(p: Path) -> Option<hir::Path> {
  p.name_dots()
    .map(|x| x.name().map(name))
    .collect::<Option<_>>()
    .map(hir::Path::new)
}

pub(crate) fn lab(l: Lab) -> Option<hir::Lab> {
  match l {
    Lab::NameLab(l) => l.name().map(|x| hir::Lab::Name(name(x))),
    Lab::IntLitLab(l) => l
      .int_lit()
      .and_then(|x| x.text().parse::<usize>().ok())
      .map(hir::Lab::Num),
  }
}

pub(crate) fn name(tok: SyntaxToken) -> hir::Name {
  hir::Name::new(tok.text())
}
