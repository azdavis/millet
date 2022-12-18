//! Test infra.

mod expect;
mod input;
mod reason;
mod show;

pub(crate) mod raw;

use diagnostic_util::Severity;

/// Given the string of an SML program with some expectation comments, panics iff the expectation
/// comments are not satisfied.
///
/// Expectation comments are regular SML comments except they:
///
/// - are always on only one line
/// - start with `(**`
/// - point at either:
///   - the specific things that should have errors with `^` or `v`
///   - lines on which an error should begin (but not necessarily end) with `+`
///
/// The expectation messages have a certain format:
///
/// - Error expects that must merely be **contained** have no prefix.
/// - Error expects that must match **exactly** begin with `exact: `.
/// - Hover expects begin with `hover: `, and the actual hover must merely contain the expectation.
///
/// To construct the string to pass without worrying about Rust string escape sequences, use the raw
/// string syntax: `r#"..."#`.
///
/// ```ignore
/// check(r#"
/// (**       vvv error about bar *)
/// val foo = bar quz
/// (**           ^^^ hover: info about quz *)
/// "#);
/// ```
///
/// Note that this also sets up logging.
#[track_caller]
pub(crate) fn check(s: &str) {
  check_multi(raw::one_file_fs(s));
}

/// Like [`check`], but allows multiple files.
#[track_caller]
pub(crate) fn check_multi<const N: usize>(files: [(&str, &str); N]) {
  raw::get(files, analysis::StdBasis::Minimal, raw::Outcome::Pass, Severity::Error);
}

/// Like [`check`], but the expectation comments should be not satisfied.
///
/// For instance, the following program has an expectation comment that doesn't make sense, since
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
/// This is useful if support for something is not implemented, but planned for later:
///
/// 1. Make a test that should eventually pass, but use `fail`.
/// 2. Later, implement the feature that test is testing.
/// 3. The test starts to actually pass, so `fail` fails.
/// 4. Update the test to use `check` instead so it actually passes.
///
/// Use `fail` instead of ignoring tests.
#[allow(dead_code)]
#[track_caller]
pub(crate) fn fail(s: &str) {
  raw::get(raw::one_file_fs(s), analysis::StdBasis::Minimal, raw::Outcome::Fail, Severity::Error);
}

/// Like [`check`], but includes the full std basis.
#[track_caller]
pub(crate) fn check_with_std_basis(s: &str) {
  raw::get(raw::one_file_fs(s), analysis::StdBasis::Full, raw::Outcome::Pass, Severity::Error);
}

/// Like [`check`] but with warnings.
#[track_caller]
pub(crate) fn check_with_warnings(s: &str) {
  raw::get(raw::one_file_fs(s), analysis::StdBasis::Minimal, raw::Outcome::Pass, Severity::Warning);
}

/// Asserts the input from the files generates an error at the given path containing the given
/// message.
#[track_caller]
pub(crate) fn check_bad_input<'a, I>(path: &str, msg: &str, files: I)
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  let (input, _) = input::get(files);
  let e = input.expect_err("unexpectedly good input");
  let got_path = e.abs_path().strip_prefix(input::ROOT.as_path()).expect("could not strip prefix");
  assert_eq!(std::path::Path::new(path), got_path, "wrong path with errors");
  let got_msg = e.display(input::ROOT.as_path()).to_string();
  assert!(got_msg.contains(msg), "want not contained in got\n  want: {msg}\n  got: {got_msg}");
}
