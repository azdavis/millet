//! Showing the result of a test.

use crate::check::{expect, reason};
use std::fmt;

pub(crate) struct Show {
  pub(crate) store: paths::Store,
  pub(crate) files: paths::PathMap<expect::File>,
  pub(crate) reasons: Vec<reason::Reason>,
}

impl Show {
  pub(crate) fn new<I>(store: paths::Store, files: I) -> Self
  where
    I: Iterator<Item = (paths::PathId, expect::File)>,
  {
    Self { store, files: files.collect(), reasons: Vec::new() }
  }
}

impl fmt::Display for Show {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("\n\n  reasons:\n")?;
    for reason in &self.reasons {
      f.write_str("  - ")?;
      match reason {
        reason::Reason::NoErrorsEmitted(want_len) => {
          writeln!(f, "wanted {want_len} errors, but got none")?;
        }
        reason::Reason::GotButNotWanted(r, got) => {
          let path = self.store.get_path(r.path).as_path().display();
          let range = r.val;
          writeln!(f, "{path}:{range}: got an error, but wanted none")?;
          writeln!(f, "    - got:  {got}")?;
        }
        reason::Reason::Mismatched(r, want, got) => {
          let path = self.store.get_path(r.path).as_path().display();
          let range = r.val;
          writeln!(f, "{path}:{range}: mismatched")?;
          writeln!(f, "    - want: {want}")?;
          writeln!(f, "    - got:  {got}")?;
        }
        reason::Reason::NoHover(r) => {
          let path = self.store.get_path(r.path).as_path().display();
          let range = r.val;
          writeln!(f, "{path}:{range}: wanted a hover, but got none")?;
        }
        reason::Reason::InvalidInexact(line, kind) => {
          let path = self.store.get_path(line.path).as_path().display();
          let line = line.val;
          writeln!(f, "{path}:{line}: inexact arrows for {kind}")?;
        }
        reason::Reason::UnexpectedlyBadInput(path, s) => {
          let path = path.display();
          writeln!(f, "{path}: unexpectedly bad input: {s}")?;
        }
        reason::Reason::UnexpectedlyGoodInput { path, msg } => {
          writeln!(f, "{path}: unexpectedly good input: no error with message: {msg}")?;
        }
        reason::Reason::WrongInputErrPath(want, got) => {
          let want = want.display();
          let got = got.display();
          writeln!(f, "{got}: wrong input error path: wanted {want}")?;
        }
        reason::Reason::InputErrMismatch(path, want, got) => {
          let path = path.display();
          writeln!(f, "{path}: mismatched input error")?;
          writeln!(f, "    - want: {want}")?;
          writeln!(f, "    - got:  {got}")?;
        }
        reason::Reason::DuplicateDef(r, d) => {
          let path = self.store.get_path(r.path).as_path().display();
          let range = r.val;
          writeln!(f, "{path}:{range}: duplicate def for `{d}`")?;
        }
        reason::Reason::Undef(r, d) => {
          let path = self.store.get_path(r.path).as_path().display();
          let range = r.val;
          writeln!(f, "{path}:{range}: `{d}` not marked as a def by any expectation comments")?;
        }
        reason::Reason::NoMatchingDef(r, d) => {
          let path = self.store.get_path(r.path).as_path().display();
          let range = r.val;
          writeln!(f, "{path}:{range}: no def here matched the def for `{d}`")?;
        }
      }
    }
    f.write_str("\n  want:")?;
    if self.files.values().all(expect::File::is_empty) {
      f.write_str(" <empty>")?;
    } else {
      f.write_str("\n")?;
      for (&path, file) in &self.files {
        let path = self.store.get_path(path).as_path().display();
        for (region, expect) in file.iter() {
          writeln!(f, "  - {path}:{region}: {expect}")?;
        }
      }
    }
    writeln!(f)?;
    Ok(())
  }
}
