use crate::types::{CMFile, Class, Error, Result, Root};
use std::path::PathBuf;

pub(crate) fn get(root: Root) -> Result<CMFile> {
  match root {
    Root::Alias(_) => Err(Error::UnsupportedAlias),
    Root::Desc(_, exports, members) => {
      let mut sml = Vec::<PathBuf>::new();
      let mut cm = Vec::<PathBuf>::new();
      for member in members {
        // NOTE: just ignore dollar paths, since we include the full basis
        if member
          .pathname
          .as_os_str()
          .to_string_lossy()
          .starts_with('$')
        {
          continue;
        }
        match member.class() {
          Some(c) => match c {
            Class::Sml => sml.push(member.pathname),
            Class::Cm => cm.push(member.pathname),
            _ => return Err(Error::UnsupportedClass(member.pathname, c)),
          },
          None => return Err(Error::CouldNotDetermineClass(member.pathname)),
        }
      }
      Ok(CMFile { exports, sml, cm })
    }
  }
}
