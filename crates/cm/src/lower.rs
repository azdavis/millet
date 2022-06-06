use crate::types;
use anyhow::{bail, Result};
use std::path::PathBuf;

pub(crate) fn get(root: types::Root) -> Result<(Vec<types::Export>, Vec<PathBuf>)> {
  match root {
    types::Root::Alias(_) => bail!("unsupported: alias"),
    types::Root::Desc(kind, exports, members) => match kind {
      types::DescKind::Group => {
        let mut ms = Vec::<PathBuf>::with_capacity(members.len());
        for member in members {
          match member.class() {
            Some(c) => match c {
              types::Class::Sml => ms.push(member.pathname),
              _ => bail!("unsupported: file with class {c:?}"),
            },
            None => bail!("unsupported: couldn't determine class"),
          }
        }
        Ok((exports, ms))
      }
      types::DescKind::Library => bail!("unsupported: library"),
    },
  }
}
