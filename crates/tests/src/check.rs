use fast_hash::FxHashMap;
use old_loc::Located;
use std::fmt;
use std::ops::Range;
use syntax::rowan::{TextRange, TextSize};

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
#[track_caller]
pub(crate) fn check(s: &str) {
  let mut cx = Cx::new(s);
  match check_impl_old(s) {
    Ok(()) => {
      if !cx.want.is_empty() {
        cx.reasons.push(Reason::NoErrorsEmitted);
      }
    }
    Err(err) => cx.add_err(Range::from(err.loc), err.val),
  }
  if let Err((range, msg)) = check_impl(s) {
    cx.add_err(Range::<usize>::from(range), msg)
  }
  if !cx.reasons.is_empty() {
    panic!("{cx}")
  }
}

enum Reason<'a> {
  WantWrongNumError,
  NoErrorsEmitted,
  CannotGetRegion(Range<usize>),
  GotButNotWanted(Region, String),
  MismatchedErrors(Region, &'a str, String),
}

struct Cx<'a> {
  indices: Vec<usize>,
  want: FxHashMap<Region, &'a str>,
  reasons: Vec<Reason<'a>>,
}

impl<'a> Cx<'a> {
  fn new(s: &'a str) -> Self {
    let want: FxHashMap<_, _> = s
      .lines()
      .enumerate()
      .filter_map(|(line_n, line_s)| get_expect_comment(line_n, line_s))
      .collect();
    let want_len = want.len();
    let mut reasons = Vec::<Reason>::new();
    if !matches!(want_len, 0 | 1) {
      reasons.push(Reason::WantWrongNumError);
    }
    Self {
      indices: s
        .bytes()
        .enumerate()
        .filter_map(|(idx, b)| (b == b'\n').then(|| idx))
        .collect(),
      want,
      reasons,
    }
  }

  fn add_err(&mut self, range: Range<usize>, got: String) {
    match get_region(&self.indices, range.clone()) {
      None => self.reasons.push(Reason::CannotGetRegion(range)),
      Some(region) => match self.want.get(&region) {
        None => self.reasons.push(Reason::GotButNotWanted(region, got)),
        Some(&want) => {
          if want != got {
            self
              .reasons
              .push(Reason::MismatchedErrors(region, want, got))
          }
        }
      },
    }
  }
}

impl fmt::Display for Cx<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("CHECK FAILED\n\nreasons:\n")?;
    let want_len = self.want.len();
    for reason in self.reasons.iter() {
      f.write_str("- ")?;
      match reason {
        Reason::WantWrongNumError => writeln!(f, "want 0 or 1 wanted errors, got {want_len}")?,
        Reason::NoErrorsEmitted => writeln!(f, "wanted {want_len} errors, but got none")?,
        Reason::CannotGetRegion(r) => writeln!(f, "couldn't get a region from {r:?}")?,
        Reason::GotButNotWanted(r, got) => {
          writeln!(f, "{r}: got an error, but wanted none")?;
          writeln!(f, "  - got:  {got}")?;
        }
        Reason::MismatchedErrors(r, want, got) => {
          writeln!(f, "{r}: mismatched errors")?;
          writeln!(f, "  - want: {want}")?;
          writeln!(f, "  - got:  {got}")?;
        }
      }
    }
    f.write_str("\nwant:")?;
    if self.want.is_empty() {
      f.write_str(" <empty>")?;
    } else {
      f.write_str("\n")?;
      for (region, &msg) in self.want.iter() {
        writeln!(f, "- {region}: {msg}")?;
      }
    }
    Ok(())
  }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Region {
  line: usize,
  col: Range<usize>,
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

fn get_region(indices: &[usize], range: Range<usize>) -> Option<Region> {
  let line = indices.iter().position(|&idx| range.start <= idx)?;
  let col = indices.get(line.checked_sub(1)?)?.checked_add(1)?;
  Some(Region {
    line,
    col: range.start.checked_sub(col)?..range.end.checked_sub(col)?,
  })
}

fn check_impl(s: &str) -> Result<(), (TextRange, String)> {
  let lexed = lex::get(s);
  if let Some(err) = lexed.errors.into_iter().next() {
    return Err((err.range, err.kind.to_string()));
  }
  let parsed = parse::get(&lexed.tokens);
  if let Some(err) = parsed.errors.into_iter().next() {
    return Err((err.range, err.kind.to_string()));
  }
  if std::env::var_os("NEW").map_or(false, |x| x == "1") {
    let lowered = lower::get(parsed.root);
    let (syms, errors) = statics::get(&lowered.arenas, &lowered.top_decs);
    if let Some(err) = errors.into_iter().next() {
      // TODO use a real range
      return Err((
        TextRange::empty(TextSize::of(s.lines().nth(1).unwrap())),
        err.display(&syms).to_string(),
      ));
    }
  }
  Ok(())
}

fn check_impl_old(s: &str) -> Result<(), Located<String>> {
  let mut store = old_intern::StrStoreMut::new();
  let lexer = match old_lex::get(&mut store, s.as_bytes()) {
    Ok(x) => x,
    Err(e) => return Err(e.loc.wrap(e.val.message())),
  };
  let store = store.finish();
  let top_decs = match old_parse::get(lexer) {
    Ok(x) => x,
    Err(e) => return Err(e.loc.wrap(e.val.message())),
  };
  let mut statics = old_statics::Statics::new();
  for top_dec in top_decs.iter() {
    match statics.get(top_dec) {
      Ok(()) => {}
      Err(e) => return Err(e.loc.wrap(e.val.message(&store))),
    }
  }
  statics.finish();
  Ok(())
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
    line,
    col: start..end,
  };
  Some((region, msg.trim_end_matches(' ')))
}
