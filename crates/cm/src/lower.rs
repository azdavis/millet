use crate::types::{CMFile, Class, Error, ErrorKind, PathKind, Result, Root};
use located::Located;
use std::path::PathBuf;

pub(crate) fn get(root: Root) -> Result<CMFile> {
  match root {
    Root::Alias(path) => Err(Error::new(ErrorKind::UnsupportedAlias, path.range)),
    Root::Desc(_, exports, members) => {
      let mut paths = Vec::<(Located<PathBuf>, PathKind)>::new();
      for member in members {
        // NOTE: just ignore dollar paths, since we include the full basis
        if member
          .pathname
          .val
          .as_os_str()
          .to_string_lossy()
          .starts_with('$')
        {
          continue;
        }
        match member.class() {
          Some(class) => match class.val {
            Class::Sml => paths.push((member.pathname, PathKind::Sml)),
            Class::Cm => paths.push((member.pathname, PathKind::Cm)),
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
        }
      }
      Ok(CMFile { exports, paths })
    }
  }
}
