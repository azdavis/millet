use old_loc::Located;
use rustc_hash::FxHashMap;
use std::ops::Range;

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
/// val foo = bar quz
/// (**           ^^^ message about quz *)
/// "#);
/// ```
pub(crate) fn check(s: &str) {
  let indices: Vec<_> = s
    .bytes()
    .enumerate()
    .filter_map(|(idx, b)| (b == b'\n').then(|| idx))
    .collect();
  let mut want: FxHashMap<_, _> = s
    .lines()
    .enumerate()
    .filter_map(|(line_n, line_s)| get_expect_comment(line_n, line_s))
    .collect();
  if let Err(err) = check_impl(s) {
    let range = Range::from(err.loc);
    let line = indices.iter().position(|&idx| range.start <= idx).unwrap();
    let col = indices[line - 1] + 1;
    let region = Region {
      line,
      col: (range.start - col)..(range.end - col),
    };
    let want_msg = want.remove(&region).unwrap();
    assert_eq!(want_msg, err.val);
  }
  assert!(want.is_empty(), "some expected errors were not emitted");
}

fn check_impl(s: &str) -> Result<(), Located<String>> {
  let mut store = old_intern::StrStoreMut::new();
  let lexer = match old_lex::get(&mut store, s.as_bytes()) {
    Ok(x) => x,
    Err(e) => return Err(e.loc.wrap(e.val.message())),
  };
  let store = store.finish();
  let top_decs = match old_parse::get(lexer) {
    Ok(x) => x,
    Err(e) => return Err(e.loc.wrap(e.val.message(&store))),
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

#[derive(Debug, PartialEq, Eq, Hash)]
struct Region {
  line: usize,
  col: Range<usize>,
}

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
