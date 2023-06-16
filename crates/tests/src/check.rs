//! Test infra.

mod expect;
mod input;
mod reason;
mod show;

pub(crate) mod raw;

use diagnostic::Severity;

/// Given the string of an SML program with some expectation comments, panics iff the expectation
/// comments are not satisfied.
///
/// Expectation comments are regular SML comments except they:
///
/// - are always on only one line
/// - start with `(**`
/// - point at either:
///   - specific things with `^` (above) or `v` (below)
///   - entire with `+` (above) or `-` (below)
///
/// The expectation messages sometimes have a certain prefix, like `hover: ` or `exact: `. These
/// cause the message following that prefix to have different meanings. Consult the tests or
/// implementation to see what prefixes are available.
///
/// Without a prefix, the expectation checks to see if there is a diagnostic emitted in the marked
/// region whose message contains the given message.
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
#[track_caller]
pub(crate) fn check(s: &str) {
  check_multi(raw::one_file_fs(s));
}

/// Like [`check`], but allows multiple files.
#[track_caller]
pub(crate) fn check_multi<const N: usize>(files: [(&str, &str); N]) {
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Minimal,
    outcome: raw::Outcome::Pass,
    limit: raw::Limit::First,
    min_severity: Severity::Error,
    expected_input: raw::ExpectedInput::Good,
  };
  raw::get(files, opts);
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
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Minimal,
    outcome: raw::Outcome::Fail,
    limit: raw::Limit::First,
    min_severity: Severity::Error,
    expected_input: raw::ExpectedInput::Good,
  };
  raw::get(raw::one_file_fs(s), opts);
}

/// Like [`check`], but includes the full std basis.
#[track_caller]
pub(crate) fn check_with_std_basis(s: &str) {
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Full,
    outcome: raw::Outcome::Pass,
    limit: raw::Limit::First,
    min_severity: Severity::Error,
    expected_input: raw::ExpectedInput::Good,
  };
  raw::get(raw::one_file_fs(s), opts);
}

/// Like [`check`] but with warnings.
#[track_caller]
pub(crate) fn check_with_warnings(s: &str) {
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Minimal,
    outcome: raw::Outcome::Pass,
    limit: raw::Limit::First,
    min_severity: Severity::Warning,
    expected_input: raw::ExpectedInput::Good,
  };
  raw::get(raw::one_file_fs(s), opts);
}

/// Asserts the input from the files generates an error at the given path containing the given
/// message.
#[track_caller]
pub(crate) fn check_bad_input<'a, I>(path: &str, msg: &str, files: I)
where
  I: IntoIterator<Item = (&'a str, &'a str)>,
{
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Minimal,
    outcome: raw::Outcome::Pass,
    limit: raw::Limit::First,
    min_severity: Severity::Warning,
    expected_input: raw::ExpectedInput::Bad { path, msg },
  };
  raw::get(files, opts);
}
