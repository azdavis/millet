//! Getting information from environments.

use crate::disallow::Disallow;
use crate::env::{Env, EnvLike};
use crate::error::{ErrorKind, Item};
use crate::types::{TyInfo, ValInfo};

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
pub(crate) fn get_env<'e, 'n, I, E>(env: &'e E, names: I) -> GetEnvResult<&'e Env>
where
  I: IntoIterator<Item = &'n str_util::Name>,
  E: EnvLike,
{
  let mut errors = Errors::default();
  let mut names = names.into_iter();
  let name = match names.next() {
    None => return GetEnvResult { val: None, errors },
    Some(x) => x,
  };
  let mut env = match env.get_str(name) {
    None => {
      errors.undefined = Some((Item::Struct, name.clone()));
      return GetEnvResult { val: None, errors };
    }
    Some(x) => x,
  };
  if let Some(d) = &env.disallow {
    errors.disallow.push((Item::Struct, d.clone(), name.clone()));
  }
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
pub(crate) fn get_ty_info<'e, E>(env: &'e E, path: &sml_hir::Path) -> GetEnvResult<&'e TyInfo>
where
  E: EnvLike,
{
  get_ty_info_raw(env, path.prefix().iter(), path.last())
}

/// does contain the undef err if there was no `last`
pub(crate) fn get_ty_info_raw<'e, 'n, S, E>(
  env: &'e E,
  prefix: S,
  last: &'n str_util::Name,
) -> GetEnvResult<&'e TyInfo>
where
  S: IntoIterator<Item = &'n str_util::Name>,
  E: EnvLike,
{
  let got_env = get_env(env, prefix);
  let mut errors = got_env.errors;
  if errors.undefined.is_some() {
    return GetEnvResult { val: None, errors };
  }
  let ty_info = match got_env.val {
    Some(env) => env.ty_env.get(last),
    None => env.get_ty(last),
  };
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
pub(crate) fn get_val_info<'e, E>(env: &'e E, path: &sml_hir::Path) -> GetEnvResult<&'e ValInfo>
where
  E: EnvLike,
{
  let got_env = get_env(env, path.prefix());
  let mut errors = got_env.errors;
  if errors.undefined.is_some() {
    return GetEnvResult { val: None, errors };
  }
  let val = match got_env.val {
    Some(e) => e.val_env.get(path.last()),
    None => env.get_val(path.last()),
  };
  if let Some(val_info) = val {
    if let Some(d) = &val_info.disallow {
      errors.disallow.push((Item::Val, d.clone(), path.last().clone()));
    }
  }
  GetEnvResult { val, errors }
}
