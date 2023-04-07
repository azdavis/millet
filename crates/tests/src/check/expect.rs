//! Expectations.

use std::{collections::BTreeMap, fmt};

/// A map from regions to expectations.
#[derive(Debug)]
pub(crate) struct File(BTreeMap<Region, Expect>);

impl File {
  pub(crate) fn new(s: &str) -> Self {
    Self(s.lines().enumerate().filter_map(|(line_n, line_s)| get_one(line_n, line_s)).collect())
  }

  pub(crate) fn get(&self, r: Region) -> Option<&Expect> {
    self.0.get(&r)
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn iter(&self) -> impl Iterator<Item = (&Region, &Expect)> + '_ {
    self.0.iter()
  }
}

/// See [`get_one`].
const COMMENT_START: &str = "(**";

/// Parses expectation comments from a line of text. The line will be the following in order:
///
/// - zero or more of any character
/// - the string `COMMENT_START` (the comment start)
/// - zero or more spaces
/// - one arrow character (^, +, v, -)
/// - zero or more non-spaces (the column range for the arrow. usually these are all the same as the
///   arrow character.)
/// - one space
/// - one or more of any character (the message)
/// - zero or more spaces
/// - the string `*)` (the comment end)
/// - zero or more of any character
///
/// If so, this returns `Some((line, col_range, msg))`, else returns `None`.
///
/// Note the arrows might be a little wonky with non-ascii.
fn get_one(line_n: usize, line_s: &str) -> Option<(Region, Expect)> {
  let (before, inner) = line_s.split_once(COMMENT_START)?;
  let (inner, _) = inner.split_once("*)")?;
  let non_space_idx = inner.find(|c| c != ' ')?;
  let inner = &inner[non_space_idx..];
  let (col_range, msg) = inner.split_once(' ')?;
  let msg = msg.trim_end_matches(' ');
  let (line, exact) = match col_range.chars().next()? {
    '^' => (line_n - 1, true),
    '+' => (line_n - 1, false),
    'v' => (line_n + 1, true),
    '-' => (line_n + 1, false),
    c => panic!("invalid arrow: {c}"),
  };
  let line = u32::try_from(line).ok()?;
  let region = if exact {
    let start = before.len() + COMMENT_START.len() + non_space_idx;
    let end = start + col_range.len();
    Region::Exact { line, col_start: u32::try_from(start).ok()?, col_end: u32::try_from(end).ok()? }
  } else {
    Region::Line(line)
  };
  Some((region, Expect::new(msg)))
}

/// A region that an expectation comment can point at.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Region {
  /// An entire line.
  Line(u32),
  /// An exact part of a line.
  Exact { line: u32, col_start: u32, col_end: u32 },
}

impl fmt::Display for Region {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    // don't add 1 for the line because the check strings usually have the first line blank.
    match self {
      Region::Exact { line, col_start, col_end } => {
        write!(f, "{line}:{}..{}", col_start + 1, col_end + 1)
      }
      Region::Line(line) => write!(f, "{line}"),
    }
  }
}

/// Something expected in a source file.
#[derive(Debug)]
pub(crate) struct Expect {
  /// The kind of expectation.
  pub(crate) kind: Kind,
  /// The message for it.
  pub(crate) msg: String,
}

impl Expect {
  fn new(msg: &str) -> Self {
    if let Some(msg) = msg.strip_prefix("hover: ") {
      return Self { kind: Kind::Hover, msg: msg.to_owned() };
    }
    if let Some(msg) = msg.strip_prefix("def: ") {
      return Self { kind: Kind::Def, msg: msg.to_owned() };
    }
    if let Some(msg) = msg.strip_prefix("use: ") {
      return Self { kind: Kind::Use, msg: msg.to_owned() };
    }
    if let Some(msg) = msg.strip_prefix("exact: ") {
      return Self { kind: Kind::Exact, msg: msg.to_owned() };
    }
    Self { kind: Kind::Contains, msg: msg.to_owned() }
  }
}

impl fmt::Display for Expect {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}: {}", self.kind, self.msg)
  }
}

/// A kind of expectation.
#[derive(Debug)]
pub(crate) enum Kind {
  /// Hovering over this should show something.
  Hover,
  /// This points at a definition site for something.
  Def,
  /// This points at a usage site for something.
  Use,
  /// There should be an error that exactly matches the given message.
  Exact,
  /// There should be an error that contains the message.
  Contains,
}

impl fmt::Display for Kind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Kind::Hover => f.write_str("hover"),
      Kind::Exact => f.write_str("exact"),
      Kind::Def => f.write_str("def"),
      Kind::Use => f.write_str("use"),
      Kind::Contains => f.write_str("contains"),
    }
  }
}
