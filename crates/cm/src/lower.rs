use crate::types;
use anyhow::{bail, Result};
use std::path::PathBuf;

pub(crate) fn get(root: types::Root) -> Result<(Vec<types::Export>, Vec<PathBuf>)> {
  match root {
    types::Root::Alias(_) => bail!("unsupported: alias"),
    types::Root::Desc(_, exports, members) => {
      let mut ms = Vec::<PathBuf>::with_capacity(members.len());
      for member in members {
        match member.class() {
          Some(c) => match c {
            types::Class::Sml => ms.push(member.pathname),
            _ => bail!("{}: unsupported class {c:?}", member.pathname.display()),
          },
          None => bail!("couldn't determine class for {}", member.pathname.display()),
        }
      }
      Ok((exports, ms))
    }
  }
}
