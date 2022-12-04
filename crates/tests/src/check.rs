//! Test infra.

mod expect;

use diagnostic_util::Severity;
use fast_hash::FxHashMap;
use once_cell::sync::Lazy;
use paths::FileSystem as _;
use std::fmt;

/// Given the string of an SML program with some expectation comments, panics iff the expectation
/// comments are not satisfied.
///
/// Expectation comments are regular SML comments except they:
///
/// - are always on only one line
/// - start with `(**`
/// - point at either:
///   - the specific things that should have errors with `^` or `v`
///   - lines on which an error should begin (but not necessarily end) with `+`
///
/// The expectation messages have a certain format:
///
/// - Error expects that must match **exactly** have no prefix.
/// - Error expects that must merely be **contained** begin with `contains: `.
/// - Hover expects begin with `hover: `, and the actual hover must merely contain the expectation.
///
/// To construct the string to pass without worrying about Rust string escape sequences, use the raw
/// string syntax: `r#"..."#`.
///
/// ```ignore
/// check(r#"
/// (**       vvv error about bar *)
/// val foo = bar quz
/// (**           ^^^ hover: info about quz *)
/// "#);
/// ```
///
/// Note that this also sets up logging.
#[track_caller]
pub(crate) fn check(s: &str) {
  check_multi(one_file_fs(s));
}

/// Like [`check`], but allows multiple files.
#[track_caller]
pub(crate) fn check_multi<const N: usize>(files: [(&str, &str); N]) {
  go(files, analysis::StdBasis::Minimal, Outcome::Pass, Severity::Error);
}

/// Like [`check`], but the expectation comments should be not satisfied.
///
/// For instance, the following program has an expectation comment that doesn't make sense, since
/// `1 + 2` should typecheck. but since `fail` expects the the comments to be unsatisfied, the test
/// passes.
///
/// ```ignore
/// fail(r#"
/// val _ = 1 + 2
/// (**     ^^^^^ contains: expected bool, found int *)
/// "#);
/// ```
///
/// This is useful if support for something is not implemented, but planned for later:
///
/// 1. Make a test that should eventually pass, but use `fail`.
/// 2. Later, implement the feature that test is testing.
/// 3. The test starts to actually pass, so `fail` fails.
/// 4. Update the test to use `check` instead so it actually passes.
///
/// Use `fail` instead of ignoring tests.
#[allow(dead_code)]
#[track_caller]
pub(crate) fn fail(s: &str) {
  go(one_file_fs(s), analysis::StdBasis::Minimal, Outcome::Fail, Severity::Error);
}

/// Like [`check`], but includes the full std basis.
#[track_caller]
pub(crate) fn check_with_std_basis(s: &str) {
  go(one_file_fs(s), analysis::StdBasis::Full, Outcome::Pass, Severity::Error);
}

/// The low-level impl that almost all top-level functions delegate to.
pub(crate) fn go<'a, I>(
  files: I,
  std_basis: analysis::StdBasis,
  want: Outcome,
  min_severity: Severity,
) where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  // ignore the Err if we already initialized logging, since that's fine.
  let (input, store) = get_input(files);
  let input = input.expect("unexpectedly bad input");
  let mut ck = Check::new(
    store,
    input.iter_sources().map(|s| {
      let file = expect::File::new(s.val);
      (s.path, file)
    }),
  );
  let want_err_len: usize = ck
    .files
    .values()
    .map(|x| {
      x.iter()
        .filter(|(_, e)| matches!(e.kind, expect::Kind::ErrorExact | expect::Kind::ErrorContains))
        .count()
    })
    .sum();
  // NOTE: we used to emit an error here if want_err_len was not 0 or 1 but no longer. this
  // allows us to write multiple error expectations. e.g. in the diagnostics tests. but note that
  // only one expectation is actually used.
  let mut an = analysis::Analysis::new(
    std_basis,
    config::ErrorLines::One,
    config::DiagnosticsFilter::None,
    false,
    true,
  );
  let err = an
    .get_many(&input)
    .into_iter()
    .flat_map(|(id, errors)| {
      errors.into_iter().filter_map(move |e| (e.severity >= min_severity).then_some((id, e)))
    })
    .next();
  for (&path, file) in &ck.files {
    for (&region, expect) in file.iter() {
      if matches!(expect.kind, expect::Kind::Hover) {
        let pos = match region {
          expect::Region::Exact { line, col_start, .. } => {
            text_pos::Position { line, character: col_start }
          }
          expect::Region::Line(n) => {
            ck.reasons.push(Reason::InexactHover(path.wrap(n)));
            continue;
          }
        };
        let r = match an.get_md(path.wrap(pos), true) {
          None => Reason::NoHover(path.wrap(region)),
          Some((got, _)) => {
            if got.contains(&expect.msg) {
              continue;
            }
            Reason::Mismatched(path.wrap(region), expect.msg.clone(), got)
          }
        };
        ck.reasons.push(r);
      }
    }
  }
  let had_error = match err {
    Some((id, e)) => {
      match get_err_reason(&ck.files, id, e.range, e.message) {
        Ok(()) => {}
        Err(r) => ck.reasons.push(r),
      }
      true
    }
    None => false,
  };
  if !had_error && want_err_len != 0 {
    ck.reasons.push(Reason::NoErrorsEmitted(want_err_len));
  }
  match (want, ck.reasons.is_empty()) {
    (Outcome::Pass, true) | (Outcome::Fail, false) => {}
    (Outcome::Pass, false) => panic!("UNEXPECTED FAIL: {ck}"),
    (Outcome::Fail, true) => panic!("UNEXPECTED PASS: {ck}"),
  }
}

/// An expected outcome from a test.
#[derive(Debug)]
pub(crate) enum Outcome {
  Pass,
  Fail,
}

/// Asserts the input from the files generates an error at the given path containing the given
/// message.
#[track_caller]
pub(crate) fn check_bad_input<'a, I>(path: &str, msg: &str, files: I)
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  let (input, _) = get_input(files);
  let e = input.expect_err("unexpectedly good input");
  let got_path = e.abs_path().strip_prefix(ROOT.as_path()).expect("could not strip prefix");
  assert_eq!(std::path::Path::new(path), got_path, "wrong path with errors");
  let got_msg = e.display(ROOT.as_path()).to_string();
  assert!(got_msg.contains(msg), "want not contained in got\n  want: {msg}\n  got: {got_msg}");
}

fn one_file_fs(s: &str) -> [(&str, &str); 2] {
  [("file.sml", s), ("sources.mlb", "file.sml")]
}

fn get_input<'a, I>(files: I) -> (analysis::input::Result, paths::Store)
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  let _ = env_logger::builder().is_test(true).try_init();
  let map: FxHashMap<_, _> = files
    .into_iter()
    .map(|(name, contents)| {
      let mut buf = ROOT.as_path().to_owned();
      buf.push(name);
      (buf, contents.to_owned())
    })
    .collect();
  let fs = paths::MemoryFileSystem::new(map);
  let mut store = paths::Store::new();
  let input = analysis::input::Input::new(&fs, &mut store, &ROOT);
  (input, store)
}

fn get_err_reason(
  files: &paths::PathMap<expect::File>,
  path: paths::PathId,
  range: text_pos::Range,
  got: String,
) -> Result<(), Reason> {
  let file = &files[&path];
  if range.start.line == range.end.line {
    let region = expect::Region::Exact {
      line: range.start.line,
      col_start: range.start.character,
      col_end: range.end.character,
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

/// The real, canonical root file system path, aka `/`. Performs IO on first access. But this
/// shouldn't fail because the root should be readable. (Otherwise, where are these tests being
/// run?)
static ROOT: Lazy<paths::CanonicalPathBuf> =
  Lazy::new(|| paths::RealFileSystem::default().canonicalize(std::path::Path::new("/")).unwrap());

struct Check {
  store: paths::Store,
  files: paths::PathMap<expect::File>,
  reasons: Vec<Reason>,
}

impl Check {
  fn new<I>(store: paths::Store, files: I) -> Self
  where
    I: Iterator<Item = (paths::PathId, expect::File)>,
  {
    Self { store, files: files.collect(), reasons: Vec::new() }
  }
}

impl fmt::Display for Check {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("\n\n  reasons:\n")?;
    for reason in &self.reasons {
      f.write_str("  - ")?;
      match reason {
        Reason::NoErrorsEmitted(want_len) => writeln!(f, "wanted {want_len} errors, but got none")?,
        Reason::GotButNotWanted(r, got) => {
          let path = self.store.get_path(r.path).as_path().display();
          let range = r.val;
          writeln!(f, "{path}:{range}: got an error, but wanted none")?;
          writeln!(f, "    - got:  {got}")?;
        }
        Reason::Mismatched(r, want, got) => {
          let path = self.store.get_path(r.path).as_path().display();
          let range = r.val;
          writeln!(f, "{path}:{range}: mismatched")?;
          writeln!(f, "    - want: {want}")?;
          writeln!(f, "    - got:  {got}")?;
        }
        Reason::NoHover(r) => {
          let path = self.store.get_path(r.path).as_path().display();
          let range = r.val;
          writeln!(f, "{path}:{range}: wanted a hover, but got none")?;
        }
        Reason::InexactHover(line) => {
          let path = self.store.get_path(line.path).as_path().display();
          let line = line.val;
          writeln!(f, "{path}:{line}: inexact arrows for hover")?;
        }
      }
    }
    f.write_str("\n  want:")?;
    if self.files.values().all(expect::File::is_empty) {
      f.write_str(" <empty>")?;
    } else {
      f.write_str("\n")?;
      for file in self.files.values() {
        for (region, expect) in file.iter() {
          writeln!(f, "  - {region}: {expect}")?;
        }
      }
    }
    writeln!(f)?;
    Ok(())
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
      expect::Kind::ErrorExact => {
        if exp.msg == got {
          Ok(true)
        } else {
          Err(Reason::Mismatched(region, exp.msg.clone(), got.to_owned()))
        }
      }
      expect::Kind::ErrorContains => {
        if got.contains(&exp.msg) {
          Ok(true)
        } else {
          Err(Reason::Mismatched(region, exp.msg.clone(), got.to_owned()))
        }
      }
      expect::Kind::Hover => Err(Reason::GotButNotWanted(region, got.to_owned())),
    },
  }
}

enum Reason {
  NoErrorsEmitted(usize),
  GotButNotWanted(paths::WithPath<expect::Region>, String),
  Mismatched(paths::WithPath<expect::Region>, String, String),
  NoHover(paths::WithPath<expect::Region>),
  InexactHover(paths::WithPath<u32>),
}
