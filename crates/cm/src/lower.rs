use crate::types::{Class, CmFile, Error, ErrorKind, ParsedPath, PathKind, Result, Root};
use located::Located;

pub(crate) fn get(root: Root) -> Result<CmFile> {
  match root {
    Root::Alias(path) => Err(Error::new(ErrorKind::UnsupportedAlias, path.range)),
    Root::Desc(_, exports, members) => {
      let mut paths = Vec::<Located<ParsedPath>>::new();
      for member in members {
        let kind = match member.class() {
          Some(class) => match class.val {
            Class::Sml => PathKind::Sml,
            Class::Cm => PathKind::Cm,
            c => {
              return Err(Error::new(
                ErrorKind::UnsupportedClass(member.pathname.val, c),
                class.range,
              ))
            }
          },
          None => {
            return Err(Error::new(
              ErrorKind::CouldNotDetermineClass(member.pathname.val),
              member.pathname.range,
            ))
          }
        };
        paths.push(Located {
          val: ParsedPath {
            path: member.pathname.val,
            kind,
          },
          range: member.pathname.range,
        });
      }
      Ok(CmFile { exports, paths })
    }
  }
}
