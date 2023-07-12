//! Helpers for working with diagnostics.

use sml_syntax::ast::AstNode as _;

/// A diagnostic.
#[derive(Debug)]
pub struct Diagnostic<R> {
  /// The range.
  pub range: R,
  /// The message.
  pub message: String,
  /// The code.
  pub code: diagnostic::Code,
  /// The severity.
  pub severity: diagnostic::Severity,
}

impl<R> Diagnostic<R> {
  /// Returns a diagnostic for the naive formatter being unable to format due to a comment at the
  /// given range.
  pub fn naive_fmt_comment(range: R) -> Diagnostic<R> {
    Diagnostic {
      range,
      message: "comment prevents formatting".to_owned(),
      code: diagnostic::Code::n(6001),
      severity: diagnostic::Severity::Warning,
    }
  }
}

/// Options for diagnostics.
#[derive(Debug, Default, Clone, Copy)]
pub struct Options {
  /// How many lines diagnostics should ideally be spread across.
  pub lines: config::DiagnosticLines,
  /// What diagnostics to ignore.
  pub ignore: config::init::DiagnosticsIgnore,
  /// What formatter should be used.
  pub format: config::init::FormatEngine,
}

/// NOTE: we used to limit the max number of diagnostics per file, but now it's trickier because not
/// all diagnostics are "errors", but it would be bad to hit the max number of diagnostics on
/// entirely warnings and then not emit the actual diagnostics. We'd need to come up with a way to
/// order the diagnostics.
pub(crate) fn source_file<F, R>(
  file: &mlb_statics::SourceFile,
  syms_tys: &sml_statics_types::St,
  options: Options,
  f: F,
) -> Vec<Diagnostic<R>>
where
  F: Fn(&text_pos::PositionDb, text_size_util::TextRange) -> Option<R>,
{
  let ignore_after_syntax = match options.ignore {
    config::init::DiagnosticsIgnore::None => false,
    config::init::DiagnosticsIgnore::AfterSyntax => true,
    config::init::DiagnosticsIgnore::All => return Vec::new(),
  };
  let mut ret: Vec<_> = std::iter::empty()
    .chain(file.syntax.lex_errors.iter().filter_map(|err| {
      let range = f(&file.syntax.pos_db, err.range())?;
      let message = err.to_string();
      Some(Diagnostic { range, message, code: err.code(), severity: err.severity() })
    }))
    .chain(file.syntax.parse.errors.iter().filter_map(|err| {
      let range = f(&file.syntax.pos_db, err.range())?;
      let message = err.to_string();
      Some(Diagnostic { range, message, code: err.code(), severity: err.severity() })
    }))
    .chain(file.syntax.lower.errors.iter().filter_map(|err| {
      let range = f(&file.syntax.pos_db, err.range())?;
      let message = err.to_string();
      Some(Diagnostic { range, message, code: err.code(), severity: err.severity() })
    }))
    .collect();
  let has_any_error = ret.iter().any(|x| matches!(x.severity, diagnostic::Severity::Error));
  if !ignore_after_syntax || !has_any_error {
    ret.extend(file.statics_errors.iter().filter_map(|err| {
      let idx = err.idx();
      let syntax = file.syntax.lower.ptrs.hir_to_ast(idx).expect("no pointer for idx");
      let node = syntax.to_node(file.syntax.parse.root.syntax());
      let range = f(&file.syntax.pos_db, sml_syntax::node_range(&node))?;
      let message = err.display(syms_tys, options.lines).to_string();
      Some(Diagnostic { range, message, code: err.code(), severity: err.severity() })
    }));
    if matches!(options.format, config::init::FormatEngine::Naive) {
      if let Err(sml_naive_fmt::Error::Comments(ranges)) =
        sml_naive_fmt::check(&file.syntax.parse.root)
      {
        ret.extend(ranges.into_iter().filter_map(|range| {
          let range = f(&file.syntax.pos_db, range)?;
          Some(Diagnostic::naive_fmt_comment(range))
        }));
      }
    }
  }
  ret
}
