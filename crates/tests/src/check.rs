use fast_hash::FxHashMap;
use std::fmt;
use std::ops::Range;
use syntax::rowan::TextRange;

/// pass the string of an SML program with some expectation comments.
///
/// expectation comments are regular SML comments except they:
/// - are always on only one line
/// - start with `(**`
/// - point at the things that should have errors
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
/// see also [`fail`] if the test is failing.
#[track_caller]
pub(crate) fn check(s: &str) {
  let cx = Check::new(&[s]);
  if !cx.reasons.is_empty() {
    panic!("{cx}")
  }
}

/// like [`check`], but the expectation comments should be not satisfied.
///
/// for instance, the following program has an expectation comment that doesn't make sense, since `1
/// + 2` should typecheck. but since `fail` expects the the comments to be unsatisfied, the test
///   passes.
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
  let cx = Check::new(&[s]);
  if cx.reasons.is_empty() {
    panic!("unexpected pass: {cx}")
  }
}

fn get_one_error(s: &str) -> Result<(), (TextRange, String)> {
  let show = env_var_yes("SHOW");
  let show = analysis::Show {
    lex: show,
    parse: show,
  };
  match analysis::get(std::iter::once(s), show)
    .pop()
    .unwrap()
    .into_iter()
    .next()
  {
    Some(x) => Err((x.range, x.message)),
    None => Ok(()),
  }
}

struct CheckFile<'a> {
  indices: Vec<usize>,
  want: FxHashMap<OneLineRegion, &'a str>,
}

struct Check<'a> {
  files: Vec<CheckFile<'a>>,
  reasons: Vec<Reason<'a>>,
}

impl<'a> Check<'a> {
  fn new(ss: &[&'a str]) -> Self {
    let mut ret = Self {
      files: ss
        .iter()
        .map(|s| CheckFile {
          indices: s
            .bytes()
            .enumerate()
            .filter_map(|(idx, b)| (b == b'\n').then(|| idx))
            .collect(),
          want: s
            .lines()
            .enumerate()
            .filter_map(|(line_n, line_s)| get_expect_comment(line_n, line_s))
            .collect(),
        })
        .collect(),
      reasons: Vec::new(),
    };
    let want_len: usize = ret.files.iter().map(|x| x.want.len()).sum();
    if !matches!(want_len, 0 | 1) {
      ret.reasons.push(Reason::WantWrongNumError(want_len));
    }
    let mut had_error = false;
    for s in ss {
      if let Err((range, msg)) = get_one_error(s) {
        had_error = true;
        ret.add_err(FileId(0), Range::<usize>::from(range), msg)
      }
    }
    if !had_error && want_len != 0 {
      ret.reasons.push(Reason::NoErrorsEmitted(want_len));
    }
    ret
  }

  fn add_err(&mut self, id: FileId, range: Range<usize>, got: String) {
    let file = &self.files[id.0];
    match get_line_col_pair(&file.indices, range.clone()) {
      None => self.reasons.push(Reason::CannotGetLineColPair(id, range)),
      Some(pair) => {
        let region = if pair.start.line == pair.end.line {
          OneLineRegion {
            line: pair.start.line,
            col: pair.start.col..pair.end.col,
          }
        } else {
          self.reasons.push(Reason::NotOneLine(id, pair));
          return;
        };
        match file.want.get(&region) {
          None => self.reasons.push(Reason::GotButNotWanted(id, region, got)),
          Some(&want) => {
            if want != got {
              self
                .reasons
                .push(Reason::MismatchedErrors(id, region, want, got))
            }
          }
        }
      }
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
    if self.files.iter().all(|x| x.want.is_empty()) {
      f.write_str(" <empty>")?;
    } else {
      f.write_str("\n")?;
      for file in self.files.iter() {
        for (region, &msg) in file.want.iter() {
          writeln!(f, "  - {region}: {msg}")?;
        }
      }
    }
    writeln!(f)?;
    Ok(())
  }
}

struct FileId(usize);

impl fmt::Display for FileId {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "file_{}.sml", self.0)
  }
}

enum Reason<'a> {
  WantWrongNumError(usize),
  NoErrorsEmitted(usize),
  CannotGetLineColPair(FileId, Range<usize>),
  NotOneLine(FileId, Range<LineCol>),
  GotButNotWanted(FileId, OneLineRegion, String),
  MismatchedErrors(FileId, OneLineRegion, &'a str, String),
}

impl<'a> fmt::Display for Reason<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Reason::WantWrongNumError(want_len) => {
        write!(f, "want 0 or 1 wanted errors, got {want_len}")
      }
      Reason::NoErrorsEmitted(want_len) => write!(f, "wanted {want_len} errors, but got none"),
      Reason::CannotGetLineColPair(file, r) => {
        write!(f, "{file}: couldn't get a line-col pair from {r:?}")
      }
      Reason::NotOneLine(file, pair) => {
        write!(f, "{file}: not one line: {}..{}", pair.start, pair.end)
      }
      Reason::GotButNotWanted(file, r, got) => {
        writeln!(f, "{file}:{r}: got an error, but wanted none")?;
        write!(f, "    - got:  {got}")
      }
      Reason::MismatchedErrors(file, r, want, got) => {
        writeln!(f, "{file}:{r}: mismatched errors")?;
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

fn get_line_col(indices: &[usize], idx: usize) -> Option<LineCol> {
  let line = indices.iter().position(|&i| idx <= i)?;
  let col_start = indices.get(line.checked_sub(1)?)?.checked_add(1)?;
  Some(LineCol {
    line,
    col: idx.checked_sub(col_start)?,
  })
}

fn get_line_col_pair(indices: &[usize], range: Range<usize>) -> Option<Range<LineCol>> {
  let start = get_line_col(indices, range.start)?;
  let end = get_line_col(indices, range.end)?;
  Some(start..end)
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

fn env_var_yes(s: &str) -> bool {
  std::env::var_os(s).map_or(false, |x| x == "1")
}
