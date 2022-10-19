use crate::types::{
  Class, CmFile, Error, ErrorKind, ParseRoot, ParsedPath, PathKind, PathOrStdBasis, Result,
};
use text_size_util::WithRange;

pub(crate) fn get(root: ParseRoot) -> Result<CmFile> {
  let mut paths = Vec::<WithRange<ParsedPath>>::new();
  for member in root.members {
    let cls = member.class();
    let path = match member.pathname.val {
      PathOrStdBasis::Path(p) => p,
      PathOrStdBasis::StdBasis => continue,
    };
    let kind = match cls {
      Some(class) => match class.val {
        Class::Sml => PathKind::Sml,
        Class::Cm => PathKind::Cm,
        Class::Other(s) => {
          return Err(Error::new(ErrorKind::UnsupportedClass(path, s), class.range))
        }
      },
      None => {
        return Err(Error::new(ErrorKind::CouldNotDetermineClass(path), member.pathname.range))
      }
    };
    paths.push(WithRange { val: ParsedPath { kind, path }, range: member.pathname.range });
  }
  Ok(CmFile { export: root.export, paths })
}
