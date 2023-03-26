//! Getting information from environments.

use crate::env::{Env, EnvLike};
use crate::error::{ErrorKind, Item};
use crate::types::{TyInfo, ValInfo};

#[derive(Debug)]
pub(crate) struct MaybeWithErrors<T> {
  pub(crate) val: Option<T>,
  pub(crate) errors: Vec<ErrorKind>,
}

impl<T> Default for MaybeWithErrors<T> {
  fn default() -> Self {
    Self { val: None, errors: Vec::new() }
  }
}

impl<T> MaybeWithErrors<T> {
  pub(crate) fn map<F, U>(self, f: F) -> MaybeWithErrors<U>
  where
    F: FnOnce(T) -> U,
  {
    MaybeWithErrors { val: self.val.map(f), errors: self.errors }
  }
}

/// uses the `names` to traverse through the `StrEnv`s of successive `env`s, returning either the
/// final `env` or an error for the first name that was unbound.
///
/// returns `Ok(None)` iff `names` was empty.
pub(crate) fn get_env<'e, 'n, I, E>(env: &'e E, names: I) -> MaybeWithErrors<&'e Env>
where
  I: IntoIterator<Item = &'n str_util::Name>,
  E: EnvLike,
{
  let mut ret = MaybeWithErrors::<&'e Env>::default();
  let mut names = names.into_iter();
  let name = match names.next() {
    None => return ret,
    Some(x) => x,
  };
  let mut env = match env.get_str(name) {
    None => {
      ret.errors.push(ErrorKind::Undefined(Item::Struct, name.clone()));
      return ret;
    }
    Some(x) => x,
  };
  for name in names {
    match env.str_env.get(name) {
      None => {
        ret.errors.push(ErrorKind::Undefined(Item::Struct, name.clone()));
        return ret;
      }
      Some(x) => env = x,
    }
  }
  ret.val = Some(env);
  ret
}

/// does contain the err if there was no `path.last()`
pub(crate) fn get_ty_info<'e, E>(env: &'e E, path: &sml_hir::Path) -> MaybeWithErrors<&'e TyInfo>
where
  E: EnvLike,
{
  get_ty_info_raw(env, path.prefix().iter(), path.last())
}

/// does contain the err if there was no `last`
pub(crate) fn get_ty_info_raw<'e, 'n, S, E>(
  env: &'e E,
  prefix: S,
  last: &'n str_util::Name,
) -> MaybeWithErrors<&'e TyInfo>
where
  S: IntoIterator<Item = &'n str_util::Name>,
  E: EnvLike,
{
  let mut got_env = get_env(env, prefix);
  let ty_info = match got_env.val {
    Some(env) => env.ty_env.get(last),
    None => env.get_ty(last),
  };
  if ty_info.is_none() {
    got_env.errors.push(ErrorKind::Undefined(Item::Ty, last.clone()));
  }
  MaybeWithErrors { val: ty_info, errors: got_env.errors }
}

/// doesn't contain the err if there was no `path.last()`
pub(crate) fn get_val_info<'e, E>(env: &'e E, path: &sml_hir::Path) -> MaybeWithErrors<&'e ValInfo>
where
  E: EnvLike,
{
  let got_env = get_env(env, path.prefix());
  let val = match got_env.val {
    Some(e) => e.val_env.get(path.last()),
    None => env.get_val(path.last()),
  };
  MaybeWithErrors { val, errors: got_env.errors }
}
