use fast_hash::FxHashMap;
use old_loc::Located;
use std::fmt;
use std::ops::Range;
use syntax::{ast::AstNode as _, rowan::TextRange};

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
  let cx = get_cx(s);
  if !cx.reasons.is_empty() {
    panic!("{cx}")
  }
}

/// like [`check`], but the expectation comments should be not satisfied.
///
/// for instance, the following program has an expectation comment that doesn't make sense, since `1
/// + 2` should typecheck. so we pass it to fail, which means the test passes.
///
/// ```ignore
/// fail(r#"
/// val _ = 1 + 2
/// (**     ^^^^^ expected bool, found int *)
/// "#);
/// ```
///
/// this is useful if support for something is not implemented, but planned for later. for instance,
/// this is a valid SML program:
///
/// ```sml
/// structure S = struct
///   val x = 1
/// end
/// val _ = S.x + 2
/// ```
///
/// however, support for modules is not implemented, so this test fails. so pass it to fail, and
/// it passes. if later support for modules _is_ implemented, that test will fail, and we can update
/// it to use [`check`] instead so it passes again.
#[track_caller]
#[allow(dead_code)]
pub(crate) fn fail(s: &str) {
  let cx = get_cx(s);
  if cx.reasons.is_empty() {
    panic!("unexpected pass: {cx}")
  }
}

fn get_cx(s: &str) -> Cx<'_> {
  let mut cx = Cx::new(s);
  match get_one_error(s) {
    Ok(()) => {
      if !cx.want.is_empty() {
        cx.reasons.push(Reason::NoErrorsEmitted(cx.want.len()));
      }
    }
    Err((range, msg)) => cx.add_err(Range::<usize>::from(range), msg),
  }
  cx
}

fn get_one_error(s: &str) -> Result<(), (TextRange, String)> {
  let show = env_var_yes("SHOW");
  let lexed = lex::get(s);
  if show {
    eprintln!("lex: {:?}", lexed.tokens);
  }
  if let Some(err) = lexed.errors.into_iter().next() {
    return Err((err.range, err.kind.to_string()));
  }
  let parsed = parse::get(&lexed.tokens);
  if show {
    eprintln!("parse: {:#?}", parsed.root);
  }
  if let Some(err) = parsed.errors.into_iter().next() {
    return Err((err.range, err.kind.to_string()));
  }
  let lowered = lower::get(&parsed.root);
  if let Some(err) = lowered.errors.into_iter().next() {
    return Err((err.range, err.kind.to_string()));
  }
  let mut st = statics::Statics::default();
  statics::get(&mut st, &lowered.arenas, &lowered.top_decs);
  if let Some(err) = st.errors.into_iter().next() {
    let ptr = lowered.ptrs.get(err.idx());
    let ptr = ptr.expect("couldn't get pointer");
    let range = ptr.to_node(parsed.root.syntax()).text_range();
    let msg = err.display(&st.syms).to_string();
    return Err((range, msg));
  }
  Ok(())
}

struct Cx<'a> {
  indices: Vec<usize>,
  want: FxHashMap<OneLineRegion, &'a str>,
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
      reasons.push(Reason::WantWrongNumError(want_len));
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
    match get_line_col_pair(&self.indices, range.clone()) {
      None => self.reasons.push(Reason::CannotGetLineColPair(range)),
      Some(pair) => {
        let region = if pair.start.line == pair.end.line {
          OneLineRegion {
            line: pair.start.line,
            col: pair.start.col..pair.end.col,
          }
        } else {
          self.reasons.push(Reason::NotOneLine(pair));
          return;
        };
        match self.want.get(&region) {
          None => self.reasons.push(Reason::GotButNotWanted(region, got)),
          Some(&want) => {
            if want != got {
              self
                .reasons
                .push(Reason::MismatchedErrors(region, want, got))
            }
          }
        }
      }
    }
  }
}

impl fmt::Display for Cx<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("CHECK FAILED\n\n  reasons:\n")?;
    for reason in self.reasons.iter() {
      writeln!(f, "  - {reason}")?;
    }
    f.write_str("\n  want:")?;
    if self.want.is_empty() {
      f.write_str(" <empty>")?;
    } else {
      f.write_str("\n")?;
      for (region, &msg) in self.want.iter() {
        writeln!(f, "  - {region}: {msg}")?;
      }
    }
    writeln!(f)?;
    Ok(())
  }
}

enum Reason<'a> {
  WantWrongNumError(usize),
  NoErrorsEmitted(usize),
  CannotGetLineColPair(Range<usize>),
  NotOneLine(Range<LineCol>),
  GotButNotWanted(OneLineRegion, String),
  MismatchedErrors(OneLineRegion, &'a str, String),
}

impl<'a> fmt::Display for Reason<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Reason::WantWrongNumError(want_len) => {
        write!(f, "want 0 or 1 wanted errors, got {want_len}")
      }
      Reason::NoErrorsEmitted(want_len) => write!(f, "wanted {want_len} errors, but got none"),
      Reason::CannotGetLineColPair(r) => write!(f, "couldn't get a line-col pair from {r:?}"),
      Reason::NotOneLine(pair) => write!(f, "not one line: {}..{}", pair.start, pair.end),
      Reason::GotButNotWanted(r, got) => {
        writeln!(f, "{r}: got an error, but wanted none")?;
        write!(f, "    - got:  {got}")
      }
      Reason::MismatchedErrors(r, want, got) => {
        writeln!(f, "{r}: mismatched errors")?;
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

#[allow(dead_code)]
fn get_one_error_old(s: &str) -> Result<(), Located<String>> {
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
