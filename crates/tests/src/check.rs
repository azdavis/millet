//! Test infra.

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
      let file = ExpectFile {
        want: s
          .val
          .lines()
          .enumerate()
          .filter_map(|(line_n, line_s)| get_expect_comment(line_n, line_s))
          .collect(),
      };
      (s.path, file)
    }),
  );
  let want_err_len: usize = ck
    .files
    .values()
    .map(|x| {
      x.want
        .iter()
        .filter(|(_, e)| matches!(e.kind, ExpectKind::ErrorExact | ExpectKind::ErrorContains))
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
    for (&region, expect) in &file.want {
      if matches!(expect.kind, ExpectKind::Hover) {
        let pos = match region {
          Region::Exact { line, col_start, .. } => {
            text_pos::Position { line, character: col_start }
          }
          Region::Line(n) => {
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
  files: &paths::PathMap<ExpectFile>,
  path: paths::PathId,
  range: text_pos::Range,
  got: String,
) -> Result<(), Reason> {
  let file = &files[&path];
  if range.start.line == range.end.line {
    let region = Region::Exact {
      line: range.start.line,
      col_start: range.start.character,
      col_end: range.end.character,
    };
    if try_region(file, path.wrap(region), got.as_str())? {
      return Ok(());
    }
  }
  let region = Region::Line(range.start.line);
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
  files: paths::PathMap<ExpectFile>,
  reasons: Vec<Reason>,
}

impl Check {
  fn new<I>(store: paths::Store, files: I) -> Self
  where
    I: Iterator<Item = (paths::PathId, ExpectFile)>,
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
    if self.files.values().all(|x| x.want.is_empty()) {
      f.write_str(" <empty>")?;
    } else {
      f.write_str("\n")?;
      for file in self.files.values() {
        for (region, expect) in &file.want {
          writeln!(f, "  - {region}: {expect}")?;
        }
      }
    }
    writeln!(f)?;
    Ok(())
  }
}

fn try_region(
  file: &ExpectFile,
  region: paths::WithPath<Region>,
  got: &str,
) -> Result<bool, Reason> {
  match file.want.get(&region.val) {
    None => Ok(false),
    Some(exp) => match exp.kind {
      ExpectKind::ErrorExact => {
        if exp.msg == got {
          Ok(true)
        } else {
          Err(Reason::Mismatched(region, exp.msg.clone(), got.to_owned()))
        }
      }
      ExpectKind::ErrorContains => {
        if got.contains(&exp.msg) {
          Ok(true)
        } else {
          Err(Reason::Mismatched(region, exp.msg.clone(), got.to_owned()))
        }
      }
      ExpectKind::Hover => Err(Reason::GotButNotWanted(region, got.to_owned())),
    },
  }
}

#[derive(Debug)]
struct ExpectFile {
  want: FxHashMap<Region, Expect>,
}

#[derive(Debug)]
struct Expect {
  msg: String,
  kind: ExpectKind,
}

impl fmt::Display for Expect {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.kind, self.msg)
  }
}

#[derive(Debug)]
enum ExpectKind {
  ErrorExact,
  ErrorContains,
  Hover,
}

impl fmt::Display for ExpectKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ExpectKind::ErrorExact => f.write_str("error (exact)"),
      ExpectKind::ErrorContains => f.write_str("error (contains)"),
      ExpectKind::Hover => f.write_str("hover (contains"),
    }
  }
}

enum Reason {
  NoErrorsEmitted(usize),
  GotButNotWanted(paths::WithPath<Region>, String),
  Mismatched(paths::WithPath<Region>, String, String),
  NoHover(paths::WithPath<Region>),
  InexactHover(paths::WithPath<u32>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Region {
  Exact { line: u32, col_start: u32, col_end: u32 },
  Line(u32),
}

impl fmt::Display for Region {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // don't add 1 for the line because the check strings usually have the first line blank.
    match self {
      Region::Exact { line, col_start, col_end } => {
        write!(f, "{}:{}..{}", line, col_start + 1, col_end + 1)
      }
      Region::Line(line) => write!(f, "{line}"),
    }
  }
}

/// See [`get_expect_comment`].
const EXPECT_COMMENT_START: &str = "(**";

/// Parses expectation comments from a line of text. The line will be the following in order:
///
/// - zero or more of any character
/// - the string `EXPECT_COMMENT_START` (the comment start)
/// - zero or more spaces
/// - one of `^` or `v` (the arrow character)
/// - zero or more non-spaces (the column range for the arrow. usually these are all the same as the
///   arrow character)
/// - one space
/// - one or more of any character (the message)
/// - zero or more spaces
/// - the string `*)` (the comment end)
/// - zero or more of any character
///
/// If so, this returns `Some((line, col_range, msg))`, else returns `None`.
///
/// Note the arrows might be a little wonky with non-ascii.
fn get_expect_comment(line_n: usize, line_s: &str) -> Option<(Region, Expect)> {
  let (before, inner) = line_s.split_once(EXPECT_COMMENT_START)?;
  let (inner, _) = inner.split_once("*)")?;
  let non_space_idx = inner.find(|c| c != ' ')?;
  let inner = &inner[non_space_idx..];
  let (col_range, msg) = inner.split_once(' ')?;
  let msg = msg.trim_end_matches(' ');
  let (line, exact) = match col_range.chars().next()? {
    '^' => (line_n - 1, true),
    '+' => (line_n - 1, false),
    'v' => (line_n + 1, true),
    c => panic!("invalid arrow: {c}"),
  };
  let line = u32::try_from(line).ok()?;
  let region = if exact {
    let start = before.len() + EXPECT_COMMENT_START.len() + non_space_idx;
    let end = start + col_range.len();
    Region::Exact { line, col_start: u32::try_from(start).ok()?, col_end: u32::try_from(end).ok()? }
  } else {
    Region::Line(line)
  };
  Some((region, get_expect(msg)))
}

fn get_expect(msg: &str) -> Expect {
  if let Some(msg) = msg.strip_prefix("contains: ") {
    return Expect { msg: msg.to_owned(), kind: ExpectKind::ErrorContains };
  }
  if let Some(msg) = msg.strip_prefix("hover: ") {
    return Expect { msg: msg.to_owned(), kind: ExpectKind::Hover };
  }
  Expect { msg: msg.to_owned(), kind: ExpectKind::ErrorExact }
}
