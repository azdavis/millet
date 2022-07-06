use crate::types::{CMFile, Class, Error, ErrorKind, Result, Root};
use located::Located;
use std::path::PathBuf;

pub(crate) fn get(root: Root) -> Result<CMFile> {
  match root {
    Root::Alias(path) => Err(Error::new(ErrorKind::UnsupportedAlias, path.range)),
    Root::Desc(_, exports, members) => {
      let mut sml = Vec::<Located<PathBuf>>::new();
      let mut cm = Vec::<Located<PathBuf>>::new();
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
            Class::Sml => sml.push(member.pathname),
            Class::Cm => cm.push(member.pathname),
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
      Ok(CMFile { exports, sml, cm })
    }
  }
}
