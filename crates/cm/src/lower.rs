use crate::types::{CMFile, Class, Error, ErrorKind, FileKind, Result, Root};
use located::Located;
use std::path::PathBuf;

pub(crate) fn get(root: Root) -> Result<CMFile> {
  match root {
    Root::Alias(path) => Err(Error::new(ErrorKind::UnsupportedAlias, path.range)),
    Root::Desc(_, exports, members) => {
      let mut files = Vec::<(Located<PathBuf>, FileKind)>::new();
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
            Class::Sml => files.push((member.pathname, FileKind::Sml)),
            Class::Cm => files.push((member.pathname, FileKind::Cm)),
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
      Ok(CMFile { exports, files })
    }
  }
}
