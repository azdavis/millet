//! Test infra.

use fast_hash::{map, FxHashMap, FxHashSet};
use std::fmt;
use std::ops::Range;
use syntax::rowan::{TextRange, TextSize};

/// pass the string of an SML program with some expectation comments.
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
  let c = Check::new(&[s], analysis::StdBasis::Minimal);
  if !c.reasons.is_empty() {
    panic!("{c}")
  }
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
  let c = Check::new(&[s], analysis::StdBasis::Minimal);
  if c.reasons.is_empty() {
    panic!("unexpected pass: {c}")
  }
}

/// like [`check`], but includes the full std basis.
#[track_caller]
pub(crate) fn check_with_std_basis(s: &str) {
  let c = Check::new(&[s], analysis::StdBasis::Full);
  if !c.reasons.is_empty() {
    panic!("{c}")
  }
}

/// like [`check`], but checks multiple files in sequence with the std basis.
#[track_caller]
pub(crate) fn check_multi(ss: &[&str]) {
  let c = Check::new(ss, analysis::StdBasis::Full);
  if !c.reasons.is_empty() {
    panic!("{c}")
  }
}

struct Check<'a> {
  files: paths::PathMap<CheckFile<'a>>,
  reasons: Vec<Reason<'a>>,
}

impl<'a> Check<'a> {
  fn new(ss: &[&'a str], std_basis: analysis::StdBasis) -> Self {
    // ignores the Err return if already initialized, since that's fine.
    let _ = simple_logger::init_with_level(log::Level::Info);
    let mut ret = Self {
      files: ss
        .iter()
        .enumerate()
        .map(|(idx, s)| {
          let file = CheckFile {
            indices: s
              .bytes()
              .enumerate()
              .filter_map(|(idx, b)| (b == b'\n').then(|| TextSize::try_from(idx).unwrap()))
              .collect(),
            want: s
              .lines()
              .enumerate()
              .filter_map(|(line_n, line_s)| get_expect_comment(line_n, line_s))
              .collect(),
          };
          (paths::PathId::from_raw(idx), file)
        })
        .collect(),
      reasons: Vec::new(),
    };
    if matches!(std_basis, analysis::StdBasis::Full) && env_var_eq_1("TEST_MINIMAL") {
      return ret;
    }
    let want_len: usize = ret.files.values().map(|x| x.want.len()).sum();
    if !matches!(want_len, 0 | 1) {
      ret.reasons.push(Reason::WantWrongNumError(want_len));
    }
    let input = analysis::Input {
      sources: ss
        .iter()
        .enumerate()
        .map(|(idx, &s)| (paths::PathId::from_raw(idx), s.to_owned()))
        .collect(),
      groups: map([(
        paths::PathId::from_raw(ss.len()),
        analysis::Group {
          source_files: (0..ss.len()).map(paths::PathId::from_raw).collect(),
          dependencies: FxHashSet::default(),
        },
      )]),
    };
    let err = analysis::Analysis::new(std_basis)
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
    range: TextRange,
    got: String,
  ) -> Result<(), Reason<'a>> {
    let file = &self.files[&id];
    let pair = match get_line_col_pair(&file.indices, range) {
      None => return Err(Reason::CannotGetLineColPair(id, range)),
      Some(x) => x,
    };
    let region = if pair.start.line == pair.end.line {
      OneLineRegion {
        line: pair.start.line,
        col: pair.start.col..pair.end.col,
      }
    } else {
      return Err(Reason::NotOneLine(id, pair));
    };
    let want = match file.want.get(&region) {
      None => return Err(Reason::GotButNotWanted(id, region, got)),
      Some(&x) => x,
    };
    if want == got {
      Ok(())
    } else {
      Err(Reason::MismatchedErrors(id, region, want, got))
    }
  }
}

impl fmt::Display for Check<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("CHECK FAILED\n\n  reasons:\n")?;
    for reason in self.reasons.iter() {
      writeln!(f, "  - {reason}")?;
    }
    f.write_str("\n  want:")?;
    if self.files.values().all(|x| x.want.is_empty()) {
      f.write_str(" <empty>")?;
    } else {
      f.write_str("\n")?;
      for file in self.files.values() {
        for (region, &msg) in file.want.iter() {
          writeln!(f, "  - {region}: {msg}")?;
        }
      }
    }
    writeln!(f)?;
    Ok(())
  }
}

struct CheckFile<'a> {
  indices: Vec<TextSize>,
  want: FxHashMap<OneLineRegion, &'a str>,
}

enum Reason<'a> {
  WantWrongNumError(usize),
  NoErrorsEmitted(usize),
  CannotGetLineColPair(paths::PathId, TextRange),
  NotOneLine(paths::PathId, Range<LineCol>),
  GotButNotWanted(paths::PathId, OneLineRegion, String),
  MismatchedErrors(paths::PathId, OneLineRegion, &'a str, String),
}

impl<'a> fmt::Display for Reason<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Reason::WantWrongNumError(want_len) => {
        write!(f, "want 0 or 1 wanted errors, got {want_len}")
      }
      Reason::NoErrorsEmitted(want_len) => write!(f, "wanted {want_len} errors, but got none"),
      Reason::CannotGetLineColPair(file, r) => {
        write!(f, "{file:?}: couldn't get a line-col pair from {r:?}")
      }
      Reason::NotOneLine(file, pair) => {
        write!(f, "{file:?}: not one line: {}..{}", pair.start, pair.end)
      }
      Reason::GotButNotWanted(file, r, got) => {
        writeln!(f, "{file:?}:{r}: got an error, but wanted none")?;
        write!(f, "    - got:  {got}")
      }
      Reason::MismatchedErrors(file, r, want, got) => {
        writeln!(f, "{file:?}:{r}: mismatched errors")?;
        writeln!(f, "    - want: {want}")?;
        write!(f, "    - got:  {got}")
      }
    }
  }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct OneLineRegion {
  line: usize,
  col: Range<usize>,
}

impl fmt::Display for OneLineRegion {
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

struct LineCol {
  line: usize,
  col: usize,
}

impl fmt::Display for LineCol {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // don't add 1 for the line because the check strings usually have the first line blank.
    write!(f, "{}:{}", self.line, self.col + 1,)
  }
}

fn get_line_col_pair(indices: &[TextSize], range: TextRange) -> Option<Range<LineCol>> {
  let start = get_line_col(indices, range.start())?;
  let end = get_line_col(indices, range.end())?;
  Some(start..end)
}

fn get_line_col(indices: &[TextSize], idx: TextSize) -> Option<LineCol> {
  let line = indices.iter().position(|&i| idx <= i)?;
  let col_start = indices
    .get(line.checked_sub(1)?)?
    .checked_add(TextSize::from(1))?;
  Some(LineCol {
    line,
    col: usize::from(idx.checked_sub(col_start)?),
  })
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
/// - one or more spaces
/// - one or more of any character (the message)
/// - zero or more spaces
/// - the string `*)` (the comment end)
/// - zero or more of any character
///
/// if yes this returns Some((line, col_range, msg)), else returns None.
///
/// note the arrows might be a little wonky with non-ascii.
fn get_expect_comment(line_n: usize, line_s: &str) -> Option<(OneLineRegion, &str)> {
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
  let region = OneLineRegion {
    line,
    col: start..end,
  };
  Some((region, msg.trim_end_matches(' ')))
}

fn env_var_eq_1(s: &str) -> bool {
  std::env::var_os(s).map_or(false, |x| x == "1")
}
