use loc::Located;
use rustc_hash::FxHashMap;
use std::ops::Range;

/// pass this the string of an sml program with some expectation comments. like this:
///
/// ```sml
/// val x = foo bar
/// (**         ^^^ message about bar *)
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
    .filter_map(|(line_n, line_s)| get_expect(line_n, line_s))
    .collect();
  if let Err(err) = check_impl(s) {
    let range = Range::from(err.loc);
    let line = indices.iter().position(|&idx| range.start <= idx).unwrap();
    let line_end = indices.iter().position(|&idx| range.end <= idx).unwrap();
    assert_eq!(line, line_end);
    let col = indices[line - 1] + 1;
    let region = Region {
      line,
      col: (range.start - col)..(range.end - col),
    };
    eprintln!("{want:?} {region:?}");
    let want_msg = want.remove(&region).unwrap();
    assert_eq!(want_msg, err.val);
  }
  assert!(want.is_empty());
}

fn check_impl(s: &str) -> Result<(), Located<String>> {
  let mut store = intern::StrStoreMut::new();
  let lexer = match lex_old::get(&mut store, s.as_bytes()) {
    Ok(x) => x,
    Err(e) => return Err(e.loc.wrap(e.val.message())),
  };
  let store = store.finish();
  let top_decs = match parse_old::get(lexer) {
    Ok(x) => x,
    Err(e) => return Err(e.loc.wrap(e.val.message(&store))),
  };
  let mut statics = statics::Statics::new();
  for top_dec in top_decs.iter() {
    match statics.get(top_dec) {
      Ok(()) => {}
      Err(e) => return Err(e.loc.wrap(e.val.message(&store))),
    }
  }
  statics.finish();
  Ok(())
}

/// see [`get_expect`].
const EXPECT_START: &str = "(**";

#[derive(Debug, PartialEq, Eq, Hash)]
struct Region {
  line: usize,
  col: Range<usize>,
}

/// parses expectation comments from a line of text. the line will be the following in order:
///
/// - zero or more of any character
/// - the string EXPECT_START (the comment start)
/// - zero or more spaces
/// - one of `^` or `v` (the arrow)
/// - zero or more non-spaces (the column range for the arrow)
/// - one or more spaces
/// - one or more of any character (the message)
/// - zero or more spaces
/// - the string `*)` (the comment end)
/// - zero or more of any character
///
/// if yes this returns Some((line, col_range, msg)), else returns None.
///
/// note the arrows might be a little wonky with non-ascii.
fn get_expect(line_n: usize, line_s: &str) -> Option<(Region, &str)> {
  let (before, inner) = line_s.split_once(EXPECT_START)?;
  let (inner, _) = inner.split_once("*)")?;
  let non_space = inner.find(|c| c != ' ')?;
  let inner = &inner[non_space..];
  let (col_range, msg) = inner.split_once(' ')?;
  let line = match inner.chars().next()? {
    '^' => line_n - 1,
    'v' => line_n + 1,
    c => panic!("invalid arrow: {c}"),
  };
  let start = before.len() + EXPECT_START.len() + non_space;
  let end = start + col_range.len();
  let region = Region {
    line,
    col: start..end,
  };
  Some((region, msg.trim()))
}
