//! Getting information from environments.

use crate::{env::Env, error::ErrorKind};
use sml_statics_types::info::{TyInfo, ValInfo};
use sml_statics_types::{disallow::Disallow, item::Item};

#[derive(Debug)]
pub(crate) struct GetEnvResult<T> {
  pub(crate) val: Result<T, UndefinedError>,
  pub(crate) disallow: Vec<DisallowError>,
}

#[derive(Debug)]
pub(crate) struct UndefinedError(Item, str_util::Name);

impl From<UndefinedError> for ErrorKind {
  fn from(val: UndefinedError) -> ErrorKind {
    ErrorKind::Undefined(val.0, val.1)
  }
}

#[derive(Debug)]
pub(crate) struct DisallowError(Item, Disallow, str_util::Name);

impl From<DisallowError> for ErrorKind {
  fn from(val: DisallowError) -> ErrorKind {
    ErrorKind::Disallowed(val.0, val.1, val.2)
  }
}

/// uses the `names` to traverse through the `StrEnv`s of successive `env`s.
pub(crate) fn get_env<'e, 'n, I>(mut env: &'e Env, names: I) -> GetEnvResult<&'e Env>
where
  I: IntoIterator<Item = &'n str_util::Name>,
{
  let mut disallow = Vec::<DisallowError>::new();
  for name in names {
    env = match env.str_env.get(name) {
      None => {
        return GetEnvResult { val: Err(UndefinedError(Item::Struct, name.clone())), disallow }
      }
      Some(x) => x,
    };
    if let Some(d) = &env.disallow {
      disallow.push(DisallowError(Item::Struct, d.clone(), name.clone()));
    }
  }
  GetEnvResult { val: Ok(env), disallow }
}

/// DOES include [`DisallowError`] from the [`TyInfo`]
pub(crate) fn get_ty_info<'e>(env: &'e Env, path: &sml_path::Path) -> GetEnvResult<&'e TyInfo> {
  get_ty_info_raw(env, path.prefix().iter(), path.last())
}

/// DOES include [`DisallowError`] from the [`TyInfo`]
pub(crate) fn get_ty_info_raw<'e, 'n, S>(
  env: &'e Env,
  prefix: S,
  last: &'n str_util::Name,
) -> GetEnvResult<&'e TyInfo>
where
  S: IntoIterator<Item = &'n str_util::Name>,
{
  let got_env = get_env(env, prefix);
  let mut disallow = got_env.disallow;
  let ty_info = match got_env.val {
    Ok(got_env) => got_env.ty_env.get(last),
    Err(e) => return GetEnvResult { val: Err(e), disallow },
  };
  let val = match ty_info {
    None => Err(UndefinedError(Item::Ty, last.clone())),
    Some(ty_info) => {
      if let Some(d) = &ty_info.disallow {
        disallow.push(DisallowError(Item::Ty, d.clone(), last.clone()));
      }
      Ok(ty_info)
    }
  };
  GetEnvResult { val, disallow }
}

/// DOES NOT include [`DisallowError`] from the [`ValInfo`]
pub(crate) fn get_val_info<'e>(env: &'e Env, path: &sml_path::Path) -> GetEnvResult<&'e ValInfo> {
  let got_env = get_env(env, path.prefix());
  let disallow = got_env.disallow;
  let val_info = match got_env.val {
    Ok(got_env) => got_env.val_env.get(path.last()),
    Err(e) => return GetEnvResult { val: Err(e), disallow },
  };
  let val = val_info.ok_or_else(|| UndefinedError(Item::Val, path.last().clone()));
  GetEnvResult { val, disallow }
}

pub(crate) fn get_mut_env<'e, 'n, I>(
  mut env: &'e mut Env,
  names: I,
) -> Result<&'e mut Env, &'n str_util::Name>
where
  I: IntoIterator<Item = &'n str_util::Name>,
{
  for name in names {
    env = match env.str_env.get_mut(name) {
      Some(x) => x,
      None => return Err(name),
    }
  }
  Ok(env)
}

pub(crate) fn get_env_raw<'e, 'n, I>(
  mut env: &'e Env,
  names: I,
) -> Result<&'e Env, &'n str_util::Name>
where
  I: IntoIterator<Item = &'n str_util::Name>,
{
  for name in names {
    env = match env.str_env.get(name) {
      Some(x) => x,
      None => return Err(name),
    }
  }
  Ok(env)
}
