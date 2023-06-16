//! Reasons for why a check failed.

use crate::check::expect;
use std::{collections::BTreeSet, path::PathBuf};

pub(crate) enum Reason {
  NoErrorsEmitted(usize),
  GotButNotWanted(paths::WithPath<expect::Region>, String),
  Mismatched(paths::WithPath<expect::Region>, String, String),
  NoHover(paths::WithPath<expect::Region>),
  InvalidInexact(paths::WithPath<u32>, expect::Kind),
  UnexpectedlyBadInput(PathBuf, String),
  UnexpectedlyGoodInput { path: String, msg: String },
  WrongInputErrPath(PathBuf, PathBuf),
  InputErrMismatch(PathBuf, String, String),
  DuplicateDef(paths::WithPath<expect::Region>, String),
  Undef(paths::WithPath<expect::Region>, String),
  NoMatchingDef(paths::WithPath<expect::Region>, String),
  MismatchedCompletions(paths::WithPath<expect::Region>, BTreeSet<String>, BTreeSet<String>),
}

pub(crate) fn get(
  files: &paths::PathMap<expect::File>,
  path: paths::PathId,
  range: text_pos::RangeUtf16,
  got: String,
) -> Result<(), Reason> {
  let file = &files[&path];
  if range.start.line == range.end.line {
    let region = expect::Region::Exact {
      line: range.start.line,
      col_start: range.start.col,
      col_end: range.end.col,
    };
    if try_region(file, path.wrap(region), got.as_str())? {
      return Ok(());
    }
  }
  let region = expect::Region::Line(range.start.line);
  if try_region(file, path.wrap(region), got.as_str())? {
    Ok(())
  } else {
    Err(Reason::GotButNotWanted(path.wrap(region), got))
  }
}

fn try_region(
  file: &expect::File,
  region: paths::WithPath<expect::Region>,
  got: &str,
) -> Result<bool, Reason> {
  match file.get(region.val) {
    None => Ok(false),
    Some(exp) => match exp.kind {
      expect::Kind::Hover
      | expect::Kind::Def
      | expect::Kind::Use
      | expect::Kind::Completions { .. } => Ok(false),
      expect::Kind::Exact => {
        if exp.msg == got {
          Ok(true)
        } else {
          Err(Reason::Mismatched(region, exp.msg.clone(), got.to_owned()))
        }
      }
      expect::Kind::Contains => {
        if got.contains(&exp.msg) {
          Ok(true)
        } else {
          Err(Reason::Mismatched(region, exp.msg.clone(), got.to_owned()))
        }
      }
    },
  }
}
