//! The "raw" test runner. Usually we use various convenient shortcuts on top of this.

use crate::check::{expect, input, reason, show};

/// An expected outcome from a test.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Outcome {
  Pass,
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

#[derive(Debug, Clone, Copy)]
pub(crate) enum ExpectedInput<'a> {
  Good,
  Bad { path: &'a str, msg: &'a str },
}

/// Options for checking.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Opts<'a> {
  pub(crate) std_basis: analysis::StdBasis,
  pub(crate) outcome: Outcome,
  pub(crate) limit: Limit,
  pub(crate) min_severity: diagnostic_util::Severity,
  pub(crate) expected_input: ExpectedInput<'a>,
}

/// The low-level impl that almost all top-level functions delegate to.
pub(crate) fn get<'a, I>(files: I, opts: Opts<'_>)
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  if matches!(opts.std_basis, analysis::StdBasis::Full) && !env_var_enabled("CI") {
    log::info!("skipping slow tests");
    return;
  }
  // ignore the Err if we already initialized logging, since that's fine.
  let (input, store) = input::get(files);
  match (opts.expected_input, input.errors.first()) {
    (ExpectedInput::Good, None) => {}
    (ExpectedInput::Good, Some(e)) => {
      panic!("unexpectedly bad input: {e:?}");
    }
    (ExpectedInput::Bad { path, msg }, None) => {
      panic!("unexpectedly good input: no error at {path} with {msg}");
    }
    (ExpectedInput::Bad { path, msg }, Some(e)) => {
      let got_path =
        e.abs_path().strip_prefix(input::ROOT.as_path()).expect("could not strip prefix");
      assert_eq!(std::path::Path::new(path), got_path, "wrong path with errors");
      let got_msg = e.display(input::ROOT.as_path()).to_string();
      assert!(got_msg.contains(msg), "want not contained in got\n  want: {msg}\n  got: {got_msg}");
    }
  }
  let mut ck = show::Show::new(
    store,
    input.iter_sources().map(|s| {
      let file = expect::File::new(s.val);
      (s.path, file)
    }),
  );
  let want_err_len: usize = ck
    .files
    .values()
    .map(|x| {
      x.iter()
        .filter(|(_, e)| matches!(e.kind, expect::Kind::ErrorExact | expect::Kind::ErrorContains))
        .count()
    })
    .sum();
  // NOTE: we used to emit an error here if want_err_len was not 0 or 1 but no longer. this
  // allows us to write multiple error expectations. e.g. in the diagnostics tests. but note that
  // only one expectation is actually used.
  let mut an = analysis::Analysis::new(opts.std_basis, config::ErrorLines::One, None, None);
  let iter = an.get_many(&input).into_iter().flat_map(|(id, errors)| {
    errors.into_iter().filter_map(move |e| (e.severity >= opts.min_severity).then_some((id, e)))
  });
  let errors: Vec<_> = match opts.limit {
    Limit::None => iter.collect(),
    Limit::First => iter.take(1).collect(),
  };
  for (&path, file) in &ck.files {
    for (&region, expect) in file.iter() {
      if matches!(expect.kind, expect::Kind::Hover) {
        let pos = match region {
          expect::Region::Exact { line, col_start, .. } => {
            text_pos::PositionUtf16 { line, col: col_start }
          }
          expect::Region::Line(n) => {
            ck.reasons.push(reason::Reason::InexactHover(path.wrap(n)));
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

pub(crate) fn one_file_fs(s: &str) -> [(&str, &str); 2] {
  [("f.sml", s), ("s.mlb", "f.sml")]
}

pub(crate) fn env_var_enabled(s: &str) -> bool {
  std::env::var_os(s).map_or(false, |x| x == "1")
}
