use crate::types::{CMFile, Class, Error, Result, Root};
use std::path::PathBuf;

pub(crate) fn get(root: Root) -> Result<CMFile> {
  match root {
    Root::Alias(_) => Err(Error::UnsupportedAlias),
    Root::Desc(_, exports, members) => {
      let mut sml = Vec::<PathBuf>::new();
      let mut cm = Vec::<PathBuf>::new();
      for member in members {
        match member.class() {
          Some(c) => match c {
            Class::Sml => sml.push(member.pathname),
            Class::CMFile => cm.push(member.pathname),
            _ => return Err(Error::UnsupportedClass(member.pathname, c)),
          },
          None => return Err(Error::UnknownClass(member.pathname)),
        }
      }
      Ok(CMFile { exports, sml, cm })
    }
  }
}
