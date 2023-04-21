//! Helpers for working with diagnostics.

use sml_syntax::ast::{self, AstNode as _};
use sml_syntax::SyntaxNode;
use text_size_util::TextRange;

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

#[derive(Debug, Clone, Copy)]
pub(crate) struct Options {
  pub(crate) lines: config::ErrorLines,
  pub(crate) ignore: Option<config::init::DiagnosticsIgnore>,
  pub(crate) format: Option<config::init::FormatEngine>,
}

/// NOTE: we used to limit the max number of diagnostics per file, but now it's trickier because not
/// all diagnostics are "errors", but it would be bad to hit the max number of diagnostics on
/// entirely warnings and then not emit the actual diagnostics. We'd need to come up with a way to
/// order the diagnostics.
pub(crate) fn source_file<F, R>(
  file: &mlb_statics::SourceFile,
  syms: &sml_statics_types::sym::Syms,
  tys: &sml_statics_types::ty::Tys,
  options: Options,
  f: F,
) -> Vec<Diagnostic<R>>
where
  F: Fn(&text_pos::PositionDb, text_size_util::TextRange) -> Option<R>,
{
  let ignore_after_syntax = match options.ignore {
    None => false,
    Some(filter) => match filter {
      config::init::DiagnosticsIgnore::AfterSyntax => true,
      config::init::DiagnosticsIgnore::All => return Vec::new(),
    },
  };
  let mut ret: Vec<_> = std::iter::empty()
    .chain(file.syntax.lex_errors.iter().filter_map(|err| {
      let range = f(&file.syntax.pos_db, err.range())?;
      let message = err.display().to_string();
      Some(Diagnostic { range, message, code: err.code(), severity: err.severity() })
    }))
    .chain(file.syntax.parse.errors.iter().filter_map(|err| {
      let range = f(&file.syntax.pos_db, err.range())?;
      let message = err.display().to_string();
      Some(Diagnostic { range, message, code: err.code(), severity: err.severity() })
    }))
    .chain(file.syntax.lower.errors.iter().filter_map(|err| {
      let range = f(&file.syntax.pos_db, err.range())?;
      let message = err.display().to_string();
      Some(Diagnostic { range, message, code: err.code(), severity: err.severity() })
    }))
    .collect();
  let has_any_error = ret.iter().any(|x| matches!(x.severity, diagnostic::Severity::Error));
  if !ignore_after_syntax || !has_any_error {
    ret.extend(file.statics_errors.iter().filter_map(|err| {
      let idx = err.idx();
      let syntax = file.syntax.lower.ptrs.hir_to_ast(idx).expect("no pointer for idx");
      let node = syntax.to_node(file.syntax.parse.root.syntax());
      let range = custom_node_range(node.clone()).unwrap_or_else(|| node.text_range());
      let range = f(&file.syntax.pos_db, range)?;
      let message = err.display(syms, tys, options.lines).to_string();
      Some(Diagnostic { range, message, code: err.code(), severity: err.severity() })
    }));
    if matches!(options.format, Some(config::init::FormatEngine::Naive)) {
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

fn custom_node_range(node: SyntaxNode) -> Option<TextRange> {
  if let Some(node) = ast::CaseExp::cast(node.clone()) {
    let case_kw = node.case_kw()?;
    let of_kw = node.of_kw()?;
    return Some(TextRange::new(case_kw.text_range().start(), of_kw.text_range().end()));
  }
  if let Some(node) = ast::LetExp::cast(node.clone()) {
    return Some(node.let_kw()?.text_range());
  }
  if let Some(node) = ast::LocalDec::cast(node.clone()) {
    return Some(node.local_dec_hd()?.local_kw()?.text_range());
  }
  if let Some(node) = ast::LetStrExp::cast(node.clone()) {
    return Some(node.let_kw()?.text_range());
  }
  if let Some(node) = ast::StructStrExp::cast(node.clone()) {
    return Some(node.struct_kw()?.text_range());
  }
  if let Some(node) = ast::SigSigExp::cast(node) {
    return Some(node.sig_kw()?.text_range());
  }
  None
}
