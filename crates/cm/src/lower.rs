use crate::types;
use anyhow::{bail, Result};
use std::path::PathBuf;

pub(crate) fn get(root: types::Root) -> Result<types::CMFile> {
  match root {
    types::Root::Alias(_) => bail!("unsupported: alias"),
    types::Root::Desc(_, exports, members) => {
      let mut sml = Vec::<PathBuf>::new();
      let mut cm = Vec::<PathBuf>::new();
      for member in members {
        match member.class() {
          Some(c) => match c {
            types::Class::Sml => sml.push(member.pathname),
            types::Class::CMFile => cm.push(member.pathname),
            _ => bail!("{}: unsupported class {c:?}", member.pathname.display()),
          },
          None => bail!("couldn't determine class for {}", member.pathname.display()),
        }
      }
      Ok(types::CMFile { exports, sml, cm })
    }
  }
}
