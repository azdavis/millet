//! Slash-variable paths.
//!
//! These paths:
//!
//! 1. have `/` as the separator
//! 2. may contain path variables starting with `$`

#[cfg(test)]
mod tests;

use fast_hash::FxHashMap;
use std::fmt;
use std::path::PathBuf;
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
      Error::Undefined(s) => write!(f, "undefined path variable: {s}"),
    }
  }
}

/// An environment for path variables.
pub type Env = FxHashMap<SmolStr, SmolStr>;

#[derive(Debug, PartialEq, Eq)]
enum Component {
  Lit(SmolStr),
  Sep,
  Var(SmolStr),
}

/// Using `/` as the path separator, this parses a path from `s`, substituting path variables (like
/// `$FOO` or `$(BAR)`) with their values in `env`.
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
      Component::Var(v) => {
        cur.push_str(env.get(v.as_str()).ok_or_else(|| Error::Undefined(v.clone()))?)
      }
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
