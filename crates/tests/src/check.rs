//! Test infra.

use fast_hash::FxHashMap;
use once_cell::sync::Lazy;
use paths::FileSystem as _;
use std::fmt::{self, Write as _};

/// Given the string of an SML program with some expectation comments, panics iff the expectation
/// comments are not satisfied.
///
/// Expectation comments are regular SML comments except they:
///
/// - are always on only one line
/// - start with `(**`
/// - point at the things that should have errors with `^` or `v`
/// - contain the expected error message for those things
///
/// To construct the string to pass without worrying about Rust string escape sequences, use the raw
/// string syntax: `r#"..."#`.
///
/// ```ignore
/// check(r#"
/// (**       vvv message about bar *)
/// val foo = bar quz
/// (**           ^^^ message about quz *)
/// "#);
/// ```
///
/// Note that this also sets up logging.
#[track_caller]
pub(crate) fn check(s: &str) {
  go(&[s], StdBasis::Minimal, Outcome::Pass)
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
/// (**     ^^^^^ expected bool, found int *)
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
  go(&[s], StdBasis::Minimal, Outcome::Fail)
}

/// Like [`check`], but includes the full std basis.
#[track_caller]
pub(crate) fn check_with_std_basis(s: &str) {
  go(&[s], StdBasis::Full, Outcome::Pass)
}

/// Like [`fail`], but includes the full std basis.
#[allow(dead_code)]
#[track_caller]
pub(crate) fn fail_with_std_basis(s: &str) {
  go(&[s], StdBasis::Full, Outcome::Fail)
}

/// Like [`check`], but checks multiple files in sequence.
#[track_caller]
pub(crate) fn check_multi(ss: &[&str]) {
  go(ss, StdBasis::Minimal, Outcome::Pass)
}

/// ignores the Err if we already initialized logging, since that's fine.
fn go(ss: &[&str], std_basis: StdBasis, want: Outcome) {
  let _ = simple_logger::init_with_level(log::Level::Info);
  if matches!(std_basis, StdBasis::Full) && env_var_eq_1("TEST_MINIMAL") {
    return;
  }
  let c = Check::new(ss, std_basis.to_analysis());
  match (want, c.reasons.is_empty()) {
    (Outcome::Pass, true) | (Outcome::Fail, false) => {}
    (Outcome::Pass, false) => panic!("UNEXPECTED FAIL: {c}"),
    (Outcome::Fail, true) => panic!("UNEXPECTED PASS: {c}"),
  }
}

enum StdBasis {
  Minimal,
  Full,
}

impl StdBasis {
  fn to_analysis(&self) -> analysis::StdBasis {
    match self {
      StdBasis::Minimal => MINIMAL.clone(),
      StdBasis::Full => FULL.clone(),
    }
  }
}

static MINIMAL: Lazy<analysis::StdBasis> = Lazy::new(analysis::StdBasis::minimal);
static FULL: Lazy<analysis::StdBasis> = Lazy::new(analysis::StdBasis::full);

/// The real, canonical root file system path, aka `/`. Performs IO on first access. But this
/// shouldn't fail because the root should be readable. (Otherwise, where are these tests being
/// run?)
pub(crate) static ROOT: Lazy<paths::CanonicalPathBuf> = Lazy::new(|| {
  paths::RealFileSystem::default()
    .canonicalize(std::path::Path::new("/"))
    .unwrap()
});

struct Check {
  root: paths::Root,
  files: paths::PathMap<CheckFile>,
  reasons: Vec<Reason>,
}

impl Check {
  fn new(ss: &[&str], std_basis: analysis::StdBasis) -> Self {
    let mut cm_file = "Group is\n".to_owned();
    let mut m = FxHashMap::<std::path::PathBuf, String>::default();
    for (idx, &s) in ss.iter().enumerate() {
      let file_name = format!("f{idx}.sml");
      writeln!(cm_file, "  {file_name}").unwrap();
      let file_name = std::path::PathBuf::from(file_name);
      m.insert(ROOT.as_path().join(file_name), s.to_owned());
    }
    m.insert(ROOT.as_path().join("sources.cm"), cm_file);
    let fs = paths::MemoryFileSystem::new(m);
    let mut root = paths::Root::new(ROOT.to_owned());
    let input =
      analysis::get_input(&fs, &mut root, None).expect("in memory fs was not set up correctly");
    let mut ret = Self {
      root,
      files: input
        .iter_sources()
        .map(|(path_id, s)| {
          let file = CheckFile {
            want: s
              .lines()
              .enumerate()
              .filter_map(|(line_n, line_s)| get_expect_comment(line_n, line_s))
              .collect(),
          };
          (path_id, file)
        })
        .collect(),
      reasons: Vec::new(),
    };
    let want_err_len: usize = ret
      .files
      .values()
      .map(|x| {
        x.want
          .iter()
          .filter(|(_, e)| matches!(e.kind, ExpectKind::Error))
          .count()
      })
      .sum();
    if !matches!(want_err_len, 0 | 1) {
      ret.reasons.push(Reason::WantWrongNumError(want_err_len));
    }
    let mut an = analysis::Analysis::new(std_basis);
    let err = an
      .get_many(&input)
      .into_iter()
      .flat_map(|(id, errors)| errors.into_iter().map(move |e| (id, e)))
      .next();
    for (&path, file) in ret.files.iter() {
      for (&region, expect) in file.want.iter() {
        if matches!(expect.kind, ExpectKind::Hover) {
          let want = format!("```sml\n{}\n```\n", expect.msg);
          let pos = analysis::Position {
            line: region.line,
            character: region.col_start,
          };
          let r = match an.get_md(path, pos) {
            None => Reason::NoHover(path, region),
            Some((got, _)) => {
              if want == got {
                continue;
              } else {
                Reason::Mismatched(path, region, want, got)
              }
            }
          };
          ret.reasons.push(r);
        }
      }
    }
    let had_error = match err {
      Some((id, e)) => {
        match ret.get_err_reason(id, e.range, e.message) {
          Ok(()) => {}
          Err(r) => ret.reasons.push(r),
        }
        true
      }
      None => false,
    };
    if !had_error && want_err_len != 0 {
      ret.reasons.push(Reason::NoErrorsEmitted(want_err_len));
    }
    ret
  }

  fn get_err_reason(
    &mut self,
    id: paths::PathId,
    range: analysis::Range,
    got: String,
  ) -> Result<(), Reason> {
    let file = &self.files[&id];
    let region = if range.start.line == range.end.line {
      Region {
        line: range.start.line,
        col_start: range.start.character,
        col_end: range.end.character,
      }
    } else {
      return Err(Reason::NotOneLine(id, range));
    };
    let want = match file.want.get(&region) {
      None => return Err(Reason::GotButNotWanted(id, region, got)),
      Some(exp) => match exp.kind {
        ExpectKind::Error => exp.msg.to_owned(),
        ExpectKind::Hover => return Err(Reason::GotButNotWanted(id, region, got)),
      },
    };
    if want == got {
      Ok(())
    } else {
      Err(Reason::Mismatched(id, region, want, got))
    }
  }
}

impl fmt::Display for Check {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("\n\n  reasons:\n")?;
    for reason in self.reasons.iter() {
      f.write_str("  - ")?;
      match reason {
        Reason::WantWrongNumError(want_len) => {
          writeln!(f, "want 0 or 1 wanted errors, got {want_len}")?;
        }
        Reason::NoErrorsEmitted(want_len) => writeln!(f, "wanted {want_len} errors, but got none")?,
        Reason::NotOneLine(path, pair) => {
          let path = self.root.get_path(*path).as_path().display();
          writeln!(f, "{path}: not one line: {}..{}", pair.start, pair.end)?;
        }
        Reason::GotButNotWanted(path, r, got) => {
          let path = self.root.get_path(*path).as_path().display();
          writeln!(f, "{path}:{r}: got an error, but wanted none")?;
          writeln!(f, "    - got:  {got}")?;
        }
        Reason::Mismatched(path, r, want, got) => {
          let path = self.root.get_path(*path).as_path().display();
          writeln!(f, "{path}:{r}: mismatched")?;
          writeln!(f, "    - want: {want}")?;
          writeln!(f, "    - got:  {got}")?;
        }
        Reason::NoHover(path, r) => {
          let path = self.root.get_path(*path).as_path().display();
          writeln!(f, "{path}:{r}: wanted a hover, but got none")?;
        }
      }
    }
    f.write_str("\n  want:")?;
    if self.files.values().all(|x| x.want.is_empty()) {
      f.write_str(" <empty>")?;
    } else {
      f.write_str("\n")?;
      for file in self.files.values() {
        for (region, expect) in file.want.iter() {
          writeln!(f, "  - {region}: {expect}")?;
        }
      }
    }
    writeln!(f)?;
    Ok(())
  }
}

enum Outcome {
  Pass,
  Fail,
}

struct CheckFile {
  want: FxHashMap<Region, Expect>,
}

struct Expect {
  msg: String,
  kind: ExpectKind,
}

impl fmt::Display for Expect {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.kind, self.msg)
  }
}

enum ExpectKind {
  Error,
  Hover,
}

impl fmt::Display for ExpectKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      ExpectKind::Error => f.write_str("error"),
      ExpectKind::Hover => f.write_str("hover"),
    }
  }
}

enum Reason {
  WantWrongNumError(usize),
  NoErrorsEmitted(usize),
  NotOneLine(paths::PathId, analysis::Range),
  GotButNotWanted(paths::PathId, Region, String),
  Mismatched(paths::PathId, Region, String, String),
  NoHover(paths::PathId, Region),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Region {
  line: u32,
  col_start: u32,
  col_end: u32,
}

impl fmt::Display for Region {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // don't add 1 for the line because the check strings usually have the first line blank.
    write!(
      f,
      "{}:{}..{}",
      self.line,
      self.col_start + 1,
      self.col_end + 1
    )
  }
}

/// See [`get_expect_comment`].
const EXPECT_COMMENT_START: &str = "(**";

/// Parses expectation comments from a line of text. The line will be the following in order:
///
/// - zero or more of any character
/// - the string EXPECT_COMMENT_START (the comment start)
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
/// If so, this returns Some((line, col_range, msg)), else returns None.
///
/// Note the arrows might be a little wonky with non-ascii.
fn get_expect_comment(line_n: usize, line_s: &str) -> Option<(Region, Expect)> {
  let (before, inner) = line_s.split_once(EXPECT_COMMENT_START)?;
  let (inner, _) = inner.split_once("*)")?;
  let non_space_idx = inner.find(|c| c != ' ')?;
  let inner = &inner[non_space_idx..];
  let (col_range, msg) = inner.split_once(' ')?;
  let line = match col_range.chars().next()? {
    '^' => line_n - 1,
    'v' => line_n + 1,
    c => panic!("invalid arrow: {c}"),
  };
  let start = before.len() + EXPECT_COMMENT_START.len() + non_space_idx;
  let end = start + col_range.len();
  let region = Region {
    line: u32::try_from(line).ok()?,
    col_start: u32::try_from(start).ok()?,
    col_end: u32::try_from(end).ok()?,
  };
  let msg = msg.trim_end_matches(' ');
  let expect = match msg.strip_prefix("hover: ") {
    Some(msg) => Expect {
      msg: msg.to_owned(),
      kind: ExpectKind::Hover,
    },
    None => Expect {
      msg: msg.to_owned(),
      kind: ExpectKind::Error,
    },
  };
  Some((region, expect))
}

fn env_var_eq_1(s: &str) -> bool {
  std::env::var_os(s).map_or(false, |x| x == "1")
}
