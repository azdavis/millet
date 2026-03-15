//! Helpers for working with diagnostics.

use sml_syntax::ast::AstNode as _;

/// A diagnostic in a file.
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
  /// More info.
  pub more_info: Vec<MoreInfo<R>>,
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
      more_info: Vec::new(),
    }
  }
}

/// More info, in the same file.
#[derive(Debug)]
pub struct MoreInfo<R> {
  /// The range in the file.
  pub range: R,
  /// The message.
  pub message: String,
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
  /// Whether to provide extra help when unifying.
  pub unify_extra_help: bool,
}

fn idx_to_range(file: &mlb_statics::SourceFile, idx: sml_hir::Idx) -> text_size_util::TextRange {
  let syntax = file.syntax.lower.ptrs.hir_to_ast(idx).expect("should have a pointer for idx");
  let node = syntax.to_node(file.syntax.parse.root.syntax());
  sml_syntax::node_range(&node)
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
      Some(Diagnostic {
        range,
        message: err.to_string(),
        code: err.code(),
        severity: err.severity(),
        more_info: Vec::new(),
      })
    }))
    .chain(file.syntax.parse.errors.iter().filter_map(|err| {
      let range = f(&file.syntax.pos_db, err.range())?;
      Some(Diagnostic {
        range,
        message: err.to_string(),
        code: err.code(),
        severity: err.severity(),
        more_info: Vec::new(),
      })
    }))
    .chain(file.syntax.lower.errors.iter().filter_map(|err| {
      let range = f(&file.syntax.pos_db, err.range())?;
      Some(Diagnostic {
        range,
        message: err.to_string(),
        code: err.code(),
        severity: err.severity(),
        more_info: Vec::new(),
      })
    }))
    .collect();
  let has_any_error = ret.iter().any(|x| matches!(x.severity, diagnostic::Severity::Error));
  if !ignore_after_syntax || !has_any_error {
    ret.extend(file.statics_errors.iter().filter_map(|err| {
      let range = f(&file.syntax.pos_db, idx_to_range(file, err.idx()))?;
      let more_info = err
        .events()
        .iter()
        .flat_map(|x| x.iter())
        .map(|ev| {
          let range = f(&file.syntax.pos_db, idx_to_range(file, ev.idx()))?;
          let message = ev.display(syms_tys).to_string();
          Some(MoreInfo { range, message })
        })
        .collect::<Option<Vec<_>>>()?;
      Some(Diagnostic {
        range,
        message: err.display(syms_tys, options.lines).to_string(),
        code: err.code(),
        severity: err.severity(),
        more_info,
      })
    }));
    if let config::init::FormatEngine::Naive = options.format
      && let Err(sml_naive_fmt::Error::Comments(ranges)) =
        sml_naive_fmt::check(&file.syntax.parse.root)
    {
      ret.extend(ranges.into_iter().filter_map(|range| {
        let range = f(&file.syntax.pos_db, range)?;
        Some(Diagnostic::naive_fmt_comment(range))
      }));
    }
  }
  ret
}
