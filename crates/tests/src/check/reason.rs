//! Reasons for why a check failed.

use crate::check::expect;
use std::path::PathBuf;

pub(crate) enum Reason {
  NoErrorsEmitted(usize),
  GotButNotWanted(paths::WithPath<expect::Region>, String),
  Mismatched(paths::WithPath<expect::Region>, String, String),
  NoHover(paths::WithPath<expect::Region>),
  InexactHover(paths::WithPath<u32>),
  UnexpectedlyBadInput(PathBuf, String),
  UnexpectedlyGoodInput { path: String, msg: String },
  WrongInputErrPath(PathBuf, PathBuf),
  InputErrMismatch(PathBuf, String, String),
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
      expect::Kind::Hover => Err(Reason::GotButNotWanted(region, got.to_owned())),
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
