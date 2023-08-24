//! Slash-variable paths.
//!
//! These paths:
//!
//! 1. have `/` as the separator
//! 2. may contain path variables starting with `$`

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

use fast_hash::FxHashMap;
use std::{fmt, path::PathBuf};
use str_util::SmolStr;

/// An error when parsing a slash var path.
#[derive(Debug)]
pub enum Error {
  /// A `$` was the last char.
  ExpectedCharAfterDollar,
  /// There was a `$(` with no matching `)`.
  ExpectedRRound,
  /// A path variable was undefined.
  Undefined(SmolStr),
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::ExpectedCharAfterDollar => f.write_str("expected a character after `$`"),
      Error::ExpectedRRound => f.write_str("expected `)`"),
      Error::Undefined(s) => write!(f, "undefined path variable: `{s}`"),
    }
  }
}

/// An un-resolved environment for path variables. Needs to know what the workspace root is to
/// resolve it.
pub type UnresolvedEnv = FxHashMap<SmolStr, EnvEntry>;

/// An environment for path variables.
pub type Env = FxHashMap<SmolStr, SmolStr>;

/// An entry in the path var env.
#[derive(Debug, Clone)]
pub struct EnvEntry {
  /// The kind of entry.
  pub kind: EnvEntryKind,
  /// The suffix, or, the entire value itself, which is also technically a suffix, depending on the
  /// kind.
  pub suffix: SmolStr,
}

/// An kind of env entry.
#[derive(Debug, Clone, Copy)]
pub enum EnvEntryKind {
  /// The `suffix` is a literal value and should not be changed.
  Value,
  /// The `suffix` should be appended to path of the parent of the workspace root.
  WorkspacePath,
}

/// Resolves an environment.
#[must_use]
pub fn resolve_env(parent: &str, env: UnresolvedEnv) -> Env {
  env
    .into_iter()
    .map(|(k, v)| {
      let v = match v.kind {
        EnvEntryKind::Value => v.suffix,
        EnvEntryKind::WorkspacePath => {
          let mut val = parent.to_owned();
          // slash is a path separator on most platforms.
          if !val.ends_with('/') {
            val.push('/');
          }
          val.push_str(v.suffix.as_str());
          val.into()
        }
      };
      (k, v)
    })
    .collect()
}

#[derive(Debug, PartialEq, Eq)]
enum Component {
  Lit(SmolStr),
  Sep,
  Var(SmolStr),
}

/// Using `/` as the path separator, this parses a path from `s`, substituting path variables (like
/// `$FOO` or `$(BAR)`) with their values in `env`.
///
/// # Errors
///
/// If there was a path variable not defined in the env.
pub fn get(s: &str, env: &Env) -> Result<PathBuf, Error> {
  let mut ret = PathBuf::new();
  let mut cur = String::new();
  for component in components(s)? {
    match component {
      Component::Lit(s) => cur.push_str(s.as_str()),
      Component::Sep => {
        // special case for absolute paths (which begin with Sep)
        if ret.as_os_str().is_empty() && cur.is_empty() {
          ret.push("/");
        } else {
          ret.push(cur.as_str());
          cur.clear();
        }
      }
      Component::Var(v) => match env.get(v.as_str()) {
        Some(x) => cur.push_str(x.as_str()),
        None => return Err(Error::Undefined(v.clone())),
      },
    }
  }
  if !cur.is_empty() {
    ret.push(cur.as_str());
  }
  Ok(ret)
}

fn components(s: &str) -> Result<Vec<Component>, Error> {
  let mut ret = Vec::<Component>::new();
  let mut cur = String::new();
  let mut chars = s.chars().peekable();
  while let Some(c) = chars.next() {
    match c {
      '$' => {
        if !cur.is_empty() {
          ret.push(Component::Lit(cur.as_str().into()));
          cur.clear();
        }
        // although `$` alone is a valid var (in CM), it cannot terminate the path.
        let &fst = chars.peek().ok_or(Error::ExpectedCharAfterDollar)?;
        let mut var = String::new();
        if fst.is_ascii_alphabetic() {
          get_var_chars(&mut var, &mut chars);
        } else if fst == '(' {
          chars.next();
          get_var_chars(&mut var, &mut chars);
          if chars.next() != Some(')') {
            return Err(Error::ExpectedRRound);
          }
        }
        // fall through, e.g. for `$/basis.cm`
        ret.push(Component::Var(var.into()));
      }
      '/' => {
        if !cur.is_empty() {
          ret.push(Component::Lit(cur.as_str().into()));
          cur.clear();
        }
        if ret.last() != Some(&Component::Sep) {
          ret.push(Component::Sep);
        }
      }
      _ => cur.push(c),
    }
  }
  if !cur.is_empty() {
    ret.push(Component::Lit(cur.into()));
  }
  Ok(ret)
}

fn get_var_chars(var: &mut String, chars: &mut std::iter::Peekable<std::str::Chars<'_>>) {
  while let Some(&c) = chars.peek() {
    if c.is_ascii_alphanumeric() || matches!(c, '-' | '_') {
      chars.next();
      var.push(c);
    } else {
      break;
    }
  }
}
