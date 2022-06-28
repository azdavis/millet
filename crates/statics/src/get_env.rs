use crate::error::{ErrorKind, Item};
use crate::types::{Env, TyInfo, ValInfo};

/// uses the `names` to traverse through the `StrEnv`s of successive `env`s, returning either the
/// final `env` or an error for the first name that was unbound.
fn get_env<'e, 'n, I>(mut env: &'e Env, names: I) -> Result<&'e Env, ErrorKind>
where
  I: IntoIterator<Item = &'n hir::Name>,
{
  for name in names {
    match env.str_env.get(name) {
      None => return Err(ErrorKind::Undefined(Item::Struct, name.clone())),
      Some(x) => env = x,
    }
  }
  Ok(env)
}

pub(crate) fn get_env_from_str_path<'e>(
  env: &'e Env,
  path: &hir::Path,
) -> Result<&'e Env, ErrorKind> {
  get_env(env, path.all_names())
}

/// returns either the ty info in the `env` reached by the `path` or an error describing why we
/// failed to do so.
pub(crate) fn get_ty_info<'e>(env: &'e Env, path: &hir::Path) -> Result<&'e TyInfo, ErrorKind> {
  get_ty_info_raw(env, path.structures().iter(), path.last())
}

pub(crate) fn get_ty_info_raw<'e, 'n, S>(
  env: &'e Env,
  structures: S,
  last: &'n hir::Name,
) -> Result<&'e TyInfo, ErrorKind>
where
  S: IntoIterator<Item = &'n hir::Name>,
{
  let ty_info = get_env(env, structures)?.ty_env.get(last);
  ty_info.ok_or_else(|| ErrorKind::Undefined(Item::Ty, last.clone()))
}

pub(crate) fn get_val_info<'e>(
  env: &'e Env,
  path: &hir::Path,
) -> Result<Option<&'e ValInfo>, ErrorKind> {
  Ok(get_env(env, path.structures())?.val_env.get(path.last()))
}
