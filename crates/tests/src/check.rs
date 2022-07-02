//! Test infra.

use fast_hash::FxHashMap;
use once_cell::sync::Lazy;
use paths::FileSystem as _;
use std::fmt::{self, Write as _};
use std::ops::Range;

/// given the string of an SML program with some expectation comments, panic iff the expectation
/// comments are not satisfied.
///
/// expectation comments are regular SML comments except they:
/// - are always on only one line
/// - start with `(**`
/// - point at the things that should have errors with `^` or `v`
/// - contain the expected error message for those things
///
/// you might want to use raw string syntax (`r#"..."#`) to construct the string to pass.
///
/// ```ignore
/// check(r#"
/// (**       vvv message about bar *)
/// val foo = bar quz
/// (**           ^^^ message about quz *)
/// "#);
/// ```
///
/// note that this also sets up logging.
#[track_caller]
pub(crate) fn check(s: &str) {
  go(&[s], StdBasis::Minimal, Outcome::Pass)
}

/// like [`check`], but the expectation comments should be not satisfied.
///
/// for instance, the following program has an expectation comment that doesn't make sense, since
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
/// this is useful if support for something is not implemented, but planned for later:
///
/// 1. make a test that should eventually pass, but use `fail`
/// 2. later, implement the feature that test is testing
/// 3. the test starts to actually pass, so `fail` fails
/// 4. update the test to use `check` instead so it actually passes
///
/// use `fail` instead of ignoring tests.
#[allow(dead_code)]
#[track_caller]
pub(crate) fn fail(s: &str) {
  go(&[s], StdBasis::Minimal, Outcome::Fail)
}

/// like [`check`], but includes the full std basis.
#[track_caller]
pub(crate) fn check_with_std_basis(s: &str) {
  go(&[s], StdBasis::Full, Outcome::Pass)
}

/// like [`check`], but checks multiple files in sequence.
#[track_caller]
pub(crate) fn check_multi(ss: &[&str]) {
  go(ss, StdBasis::Minimal, Outcome::Pass)
}

fn go(ss: &[&str], std_basis: StdBasis, want: Outcome) {
  // ignores the Err return if already initialized, since that's fine.
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

/// the real, canonical root FS path. performs IO on first access. but this shouldn't fail because
/// `/` should be readable.
static ROOT: Lazy<paths::CanonicalPathBuf> = Lazy::new(|| {
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
      writeln!(cm_file, "  f{idx}.sml").unwrap();
      m.insert(format!("/f{idx}.sml").into(), s.to_owned());
    }
    m.insert(ROOT.as_path().join(analysis::ROOT_GROUP), cm_file);
    let fs = paths::MemoryFileSystem::new(m);
    let mut root = paths::Root::new(ROOT.to_owned());
    let input = analysis::get_input(&fs, &mut root).expect("in memory fs was not set up correctly");
    let mut ret = Self {
      root,
      files: input
        .iter_sources()
        .map(|(path_id, s)| {
          let file = CheckFile {
            want: s
              .lines()
              .enumerate()
              .filter_map(|(line_n, line_s)| {
                let (a, b) = get_expect_comment(line_n, line_s)?;
                Some((a, b.to_owned()))
              })
              .collect(),
          };
          (path_id, file)
        })
        .collect(),
      reasons: Vec::new(),
    };
    let want_len: usize = ret.files.values().map(|x| x.want.len()).sum();
    if !matches!(want_len, 0 | 1) {
      ret.reasons.push(Reason::WantWrongNumError(want_len));
    }
    let mut an = analysis::Analysis::new(std_basis);
    let err = an
      .get_many(&input)
      .into_iter()
      .flat_map(|(id, errors)| errors.into_iter().map(move |e| (id, e)))
      .next();
    let had_error = match err {
      Some((id, e)) => {
        match ret.get_reason(id, e.range, e.message) {
          Ok(()) => {}
          Err(r) => ret.reasons.push(r),
        }
        true
      }
      None => false,
    };
    if !had_error && want_len != 0 {
      ret.reasons.push(Reason::NoErrorsEmitted(want_len));
    }
    ret
  }

  fn get_reason(
    &mut self,
    id: paths::PathId,
    range: analysis::Range,
    got: String,
  ) -> Result<(), Reason> {
    let file = &self.files[&id];
    let region = if range.start.line == range.end.line {
      Region {
        line: range.start.line,
        col: range.start.character..range.end.character,
      }
    } else {
      return Err(Reason::NotOneLine(id, range));
    };
    let want = match file.want.get(&region) {
      None => return Err(Reason::GotButNotWanted(id, region, got)),
      Some(x) => x.to_owned(),
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
      }
    }
    f.write_str("\n  want:")?;
    if self.files.values().all(|x| x.want.is_empty()) {
      f.write_str(" <empty>")?;
    } else {
      f.write_str("\n")?;
      for file in self.files.values() {
        for (region, msg) in file.want.iter() {
          writeln!(f, "  - {region}: {msg}")?;
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
  want: FxHashMap<Region, String>,
}

enum Reason {
  WantWrongNumError(usize),
  NoErrorsEmitted(usize),
  NotOneLine(paths::PathId, analysis::Range),
  GotButNotWanted(paths::PathId, Region, String),
  Mismatched(paths::PathId, Region, String, String),
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Region {
  line: u32,
  col: Range<u32>,
}

impl fmt::Display for Region {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // don't add 1 for the line because the check strings usually have the first line blank.
    write!(
      f,
      "{}:{}..{}",
      self.line,
      self.col.start + 1,
      self.col.end + 1
    )
  }
}

/// see [`get_expect_comment`].
const EXPECT_COMMENT_START: &str = "(**";

/// parses expectation comments from a line of text. the line will be the following in order:
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
/// if yes this returns Some((line, col_range, msg)), else returns None.
///
/// note the arrows might be a little wonky with non-ascii.
fn get_expect_comment(line_n: usize, line_s: &str) -> Option<(Region, &str)> {
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
    col: u32::try_from(start).ok()?..u32::try_from(end).ok()?,
  };
  Some((region, msg.trim_end_matches(' ')))
}

fn env_var_eq_1(s: &str) -> bool {
  std::env::var_os(s).map_or(false, |x| x == "1")
}
