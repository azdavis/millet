//! Getting information from environments.

use crate::env::{Env, EnvLike};
use crate::error::{ErrorKind, Item};
use crate::types::{TyInfo, ValInfo};

/// uses the `names` to traverse through the `StrEnv`s of successive `env`s, returning either the
/// final `env` or an error for the first name that was unbound.
///
/// returns `Ok(None)` iff `names` was empty.
fn get_env<'e, 'n, I, E>(env: &'e E, names: I) -> Result<Option<&'e Env>, ErrorKind>
where
  I: IntoIterator<Item = &'n str_util::Name>,
  E: EnvLike,
{
  let mut names = names.into_iter();
  let name = match names.next() {
    None => return Ok(None),
    Some(x) => x,
  };
  let mut env = match env.get_str(name) {
    None => return Err(ErrorKind::Undefined(Item::Struct, name.clone())),
    Some(x) => x,
  };
  for name in names {
    match env.str_env.get(name) {
      None => return Err(ErrorKind::Undefined(Item::Struct, name.clone())),
      Some(x) => env = x,
    }
  }
  Ok(Some(env))
}

pub(crate) fn get_env_from_str_path<'e, E>(
  env: &'e E,
  path: &sml_hir::Path,
) -> Result<&'e Env, ErrorKind>
where
  E: EnvLike,
{
  Ok(get_env(env, path.all_names())?.expect("path must have at least one name"))
}

/// returns either the ty info in the `env` reached by the `path` or an error describing why we
/// failed to do so.
pub(crate) fn get_ty_info<'e, E>(env: &'e E, path: &sml_hir::Path) -> Result<&'e TyInfo, ErrorKind>
where
  E: EnvLike,
{
  get_ty_info_raw(env, path.prefix().iter(), path.last())
}

pub(crate) fn get_ty_info_raw<'e, 'n, S, E>(
  env: &'e E,
  prefix: S,
  last: &'n str_util::Name,
) -> Result<&'e TyInfo, ErrorKind>
where
  S: IntoIterator<Item = &'n str_util::Name>,
  E: EnvLike,
{
  let got_env = get_env(env, prefix)?;
  let ty_info = match got_env {
    Some(got_env) => got_env.ty_env.get(last),
    None => env.get_ty(last),
  };
  ty_info.ok_or_else(|| ErrorKind::Undefined(Item::Ty, last.clone()))
}

pub(crate) fn get_val_info<'e, E>(
  env: &'e E,
  path: &sml_hir::Path,
) -> Result<Option<&'e ValInfo>, ErrorKind>
where
  E: EnvLike,
{
  match get_env(env, path.prefix())? {
    Some(e) => Ok(e.val_env.get(path.last())),
    None => Ok(env.get_val(path.last())),
  }
}
