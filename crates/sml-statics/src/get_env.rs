//! Getting information from environments.

use crate::types::{TyInfo, ValInfo};
use crate::{disallow::Disallow, env::Env, error::ErrorKind, item::Item};

#[derive(Debug)]
pub(crate) struct GetEnvResult<T> {
  pub(crate) val: Option<T>,
  pub(crate) errors: Errors,
}

impl<T> GetEnvResult<T> {
  pub(crate) fn map<F, U>(self, f: F) -> GetEnvResult<U>
  where
    F: FnOnce(T) -> U,
  {
    GetEnvResult { val: self.val.map(f), errors: self.errors }
  }
}

#[derive(Debug, Default)]
pub(crate) struct Errors {
  undefined: Option<(Item, str_util::Name)>,
  disallow: Vec<(Item, Disallow, str_util::Name)>,
}

impl Iterator for Errors {
  type Item = ErrorKind;
  fn next(&mut self) -> Option<Self::Item> {
    if let Some((a, b)) = self.undefined.take() {
      return Some(ErrorKind::Undefined(a, b));
    }
    if let Some((a, b, c)) = self.disallow.pop() {
      return Some(ErrorKind::Disallowed(a, b, c));
    }
    None
  }
}

/// uses the `names` to traverse through the `StrEnv`s of successive `env`s.
pub(crate) fn get_env<'e, 'n, I>(mut env: &'e Env, names: I) -> GetEnvResult<&'e Env>
where
  I: IntoIterator<Item = &'n str_util::Name>,
{
  let mut errors = Errors::default();
  for name in names {
    env = match env.str_env.get(name) {
      None => {
        errors.undefined = Some((Item::Struct, name.clone()));
        return GetEnvResult { val: None, errors };
      }
      Some(x) => x,
    };
    if let Some(d) = &env.disallow {
      errors.disallow.push((Item::Struct, d.clone(), name.clone()));
    }
  }
  GetEnvResult { val: Some(env), errors }
}

/// does contain the undef err if there was no `path.last()`
pub(crate) fn get_ty_info<'e>(env: &'e Env, path: &sml_path::Path) -> GetEnvResult<&'e TyInfo> {
  get_ty_info_raw(env, path.prefix().iter(), path.last())
}

/// does contain the undef err if there was no `last`
pub(crate) fn get_ty_info_raw<'e, 'n, S>(
  env: &'e Env,
  prefix: S,
  last: &'n str_util::Name,
) -> GetEnvResult<&'e TyInfo>
where
  S: IntoIterator<Item = &'n str_util::Name>,
{
  let got_env = get_env(env, prefix);
  let mut errors = got_env.errors;
  if errors.undefined.is_some() {
    return GetEnvResult { val: None, errors };
  }
  let ty_info = got_env.val.unwrap_or(env).ty_env.get(last);
  match ty_info {
    None => errors.undefined = Some((Item::Ty, last.clone())),
    Some(ty_info) => {
      if let Some(d) = &ty_info.disallow {
        errors.disallow.push((Item::Ty, d.clone(), last.clone()));
      }
    }
  };
  GetEnvResult { val: ty_info, errors }
}

/// doesn't contain the undef err if there was no `path.last()`
pub(crate) fn get_val_info<'e>(env: &'e Env, path: &sml_path::Path) -> GetEnvResult<&'e ValInfo> {
  let got_env = get_env(env, path.prefix());
  let mut errors = got_env.errors;
  if errors.undefined.is_some() {
    return GetEnvResult { val: None, errors };
  }
  let val = got_env.val.unwrap_or(env).val_env.get(path.last());
  if let Some(val_info) = val {
    if let Some(d) = &val_info.disallow {
      errors.disallow.push((Item::Val, d.clone(), path.last().clone()));
    }
  }
  GetEnvResult { val, errors }
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
