//! A panic hook with good backtraces and an exhortation to file a bug report.

const VERSION: &str = env!("CARGO_PKG_VERSION");
const ISSUES_URL: &str = "https://github.com/azdavis/millet/issues";

/// Installs the hook. Call this at the beginning of `fn main()`.
pub fn install() {
  let msg = format!("Millet ({VERSION}) crashed. We would appreciate a bug report: {ISSUES_URL}");
  better_panic::Settings::new().message(msg).verbosity(better_panic::Verbosity::Medium).install();
}
