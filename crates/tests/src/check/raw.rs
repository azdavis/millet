//! The "raw" test runner. Usually we use various convenient shortcuts on top of this.

use fast_hash::FxHashMap;
use once_cell::sync::Lazy;

use crate::check::{expect, input, reason, show};

/// A std basis.
#[derive(Debug, Clone, Copy)]
pub(crate) enum StdBasis {
  /// The minimal one.
  Minimal,
  /// The full one.
  Full,
}

/// An expected outcome from a test.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Outcome {
  /// The test should pass.
  Pass,
  /// The test should fail.
  ///
  /// We don't actually want tests to fail. But this is useful for recording that there's a known
  /// bug or unimplemented bit. Then if we implement it, the test expecting failure will fail, i.e.
  /// it'll pass, and we'll be reminded to update the test.
  Fail,
}

/// How to limit checking errors.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Limit {
  /// No limit, i.e. check all.
  None,
  /// Only check the first.
  First,
}

/// What we expect the input to be.
#[derive(Debug, Clone, Copy)]
pub(crate) enum ExpectedInput<'a> {
  /// The input itself is good.
  ///
  /// Most tests use this, since we're usually testing later "stages" (parsing, typechecking, etc).
  Good,
  /// The input is bad. There should be an error about a certain path containing a certain message.
  Bad { path: &'a str, msg: &'a str },
}

/// Options for checking.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Opts<'a> {
  pub(crate) std_basis: StdBasis,
  pub(crate) outcome: Outcome,
  pub(crate) limit: Limit,
  pub(crate) min_severity: diagnostic::Severity,
  pub(crate) expected_input: ExpectedInput<'a>,
}

/// The low-level impl that almost all top-level "check" functions delegate to.
#[track_caller]
#[allow(clippy::too_many_lines)]
pub(crate) fn get<'a, I>(files: I, opts: Opts<'_>)
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  if matches!(opts.std_basis, StdBasis::Full) && env_var_enabled("SKIP_FULL_STD_BASIS") {
    log::info!("skipping full std basis tests");
    return;
  }
  let (input, store) = input::get(files);
  let mut ck =
    show::Show::new(store, input.sources.iter().map(|(&path, s)| (path, expect::File::new(s))));
  match (opts.expected_input, input.errors.first()) {
    (ExpectedInput::Good, None) => {}
    (ExpectedInput::Good, Some(e)) => {
      let got_path =
        e.abs_path().strip_prefix(input::ROOT.as_path()).expect("could not strip ROOT prefix");
      let msg = e.display(input::ROOT.as_path()).to_string();
      ck.reasons.push(reason::Reason::UnexpectedlyBadInput(got_path.to_owned(), msg));
    }
    (ExpectedInput::Bad { path, msg }, None) => {
      let path = path.to_owned();
      let msg = msg.to_owned();
      ck.reasons.push(reason::Reason::UnexpectedlyGoodInput { path, msg });
    }
    (ExpectedInput::Bad { path, msg }, Some(e)) => {
      let got_path =
        e.abs_path().strip_prefix(input::ROOT.as_path()).expect("could not strip ROOT prefix");
      let path = std::path::Path::new(path);
      if path != got_path {
        ck.reasons.push(reason::Reason::WrongInputErrPath(path.to_owned(), got_path.to_owned()));
      }
      let got_msg = e.display(input::ROOT.as_path()).to_string();
      if !got_msg.contains(msg) {
        ck.reasons.push(reason::Reason::InputErrMismatch(
          got_path.to_owned(),
          msg.to_owned(),
          got_msg,
        ));
      }
    }
  }
  let want_err_len = ck
    .files
    .values()
    .flat_map(expect::File::iter)
    .filter(|(_, e)| matches!(e.kind, expect::Kind::Exact | expect::Kind::Contains))
    .count();
  let std_basis = match opts.std_basis {
    StdBasis::Minimal => analysis::StdBasis::minimal(),
    StdBasis::Full => FULL.clone(),
  };
  // NOTE: we used to emit an error here if want_err_len was not 0 or 1 but no longer. this
  // allows us to write multiple error expectations. e.g. in the diagnostics tests. but note that
  // only one expectation is actually used.
  let mut an = analysis::Analysis::new(std_basis, config::ErrorLines::One, None, None);
  let iter = an.get_many(&input).into_iter().flat_map(|(id, errors)| {
    errors.into_iter().filter_map(move |e| (e.severity >= opts.min_severity).then_some((id, e)))
  });
  let errors: Vec<_> = match opts.limit {
    Limit::None => iter.collect(),
    Limit::First => iter.take(1).collect(),
  };
  let mut defs = FxHashMap::<&str, expect::Region>::default();
  for (&path, file) in &ck.files {
    defs.clear();
    for (&region, expect) in file.iter() {
      match expect.kind {
        expect::Kind::Hover => {
          let pos = match region {
            expect::Region::Exact { line, col_start, .. } => {
              text_pos::PositionUtf16 { line, col: col_start }
            }
            expect::Region::Line(n) => {
              ck.reasons.push(reason::Reason::InvalidInexact(path.wrap(n), expect::Kind::Hover));
              continue;
            }
          };
          let r = match an.get_md(path.wrap(pos), true) {
            None => reason::Reason::NoHover(path.wrap(region)),
            Some((got, _)) => {
              if got.contains(&expect.msg) {
                continue;
              }
              reason::Reason::Mismatched(path.wrap(region), expect.msg.clone(), got)
            }
          };
          ck.reasons.push(r);
        }
        expect::Kind::Def => {
          if defs.insert(expect.msg.as_str(), region).is_some() {
            let r = reason::Reason::DuplicateDef(path.wrap(region), expect.msg.clone());
            ck.reasons.push(r);
          }
        }
        expect::Kind::Use => {
          let pos = match region {
            expect::Region::Exact { line, col_start, .. } => {
              text_pos::PositionUtf16 { line, col: col_start }
            }
            expect::Region::Line(n) => {
              ck.reasons.push(reason::Reason::InvalidInexact(path.wrap(n), expect::Kind::Use));
              continue;
            }
          };
          let got_defs = an.get_defs(path.wrap(pos));
          match defs.get(expect.msg.as_str()) {
            Some(&def) => {
              let any_def_matches = got_defs.iter().flatten().any(|&gd| {
                if gd.path != path {
                  return false;
                }
                let start = gd.val.start;
                let end = gd.val.end;
                match def {
                  expect::Region::Exact { line, col_start, col_end } => {
                    start.line == line
                      && end.line == line
                      && start.col == col_start
                      && end.col == col_end
                  }
                  expect::Region::Line(n) => start.line == n && end.line == n,
                }
              });
              if !any_def_matches {
                let r = reason::Reason::NoMatchingDef(path.wrap(region), expect.msg.clone());
                ck.reasons.push(r);
              }
            }
            None => {
              let r = reason::Reason::Undef(path.wrap(region), expect.msg.clone());
              ck.reasons.push(r);
            }
          }
        }
        expect::Kind::Exact | expect::Kind::Contains => {}
      }
    }
  }
  if errors.is_empty() && want_err_len != 0 {
    ck.reasons.push(reason::Reason::NoErrorsEmitted(want_err_len));
  }
  for (id, e) in errors {
    match reason::get(&ck.files, id, e.range, e.message) {
      Ok(()) => {}
      Err(r) => ck.reasons.push(r),
    }
  }
  match (opts.outcome, ck.reasons.is_empty()) {
    (Outcome::Pass, true) | (Outcome::Fail, false) => {}
    (Outcome::Pass, false) => panic!("UNEXPECTED FAIL: {ck}"),
    (Outcome::Fail, true) => panic!("UNEXPECTED PASS: {ck}"),
  }
}

/// Returns a simple "filesystem" of a single source file and a single group file pointing to that
/// source file.
pub(crate) fn one_file_fs(s: &str) -> [(&str, &str); 2] {
  [("s.mlb", "f.sml"), ("f.sml", s)]
}

/// Returns whether the env var is set to `1`.
pub(crate) fn env_var_enabled(s: &str) -> bool {
  std::env::var_os(s).map_or(false, |x| x == "1")
}

static FULL: Lazy<analysis::StdBasis> = Lazy::new(analysis::StdBasis::full);
