//! Helpers for working with diagnostics.

use diagnostic_util::Diagnostic;
use sml_syntax::ast::{self, AstNode as _};
use sml_syntax::SyntaxNode;
use std::fmt;
use text_size_util::TextRange;

#[derive(Debug, Clone, Copy)]
pub(crate) struct Options {
  pub(crate) lines: config::ErrorLines,
  pub(crate) filter: config::DiagnosticsFilter,
  pub(crate) format: bool,
}

fn diagnostic<M>(
  file: &mlb_statics::SourceFile,
  severities: &input::Severities,
  range: TextRange,
  message: M,
  code: diagnostic_util::Code,
  severity: diagnostic_util::Severity,
) -> Option<Diagnostic>
where
  M: fmt::Display,
{
  let severity = match severities.get(&code) {
    Some(&Some(sev)) => sev,
    Some(None) => return None,
    None => severity,
  };
  Some(Diagnostic {
    range: file.syntax.pos_db.range(range)?,
    message: message.to_string(),
    code,
    severity,
  })
}

/// TODO: we used to limit the max number of diagnostics per file, but now it's trickier because not
/// all diagnostics are "errors", but it would be bad to hit the max number of diagnostics on
/// entirely warnings and then not emit the actual diagnostics. We'd need to come up with a way to
/// order the diagnostics.
pub(crate) fn source_file(
  file: &mlb_statics::SourceFile,
  syms: &sml_statics::Syms,
  severities: &input::Severities,
  options: Options,
) -> Vec<Diagnostic> {
  let mut ret: Vec<_> = std::iter::empty()
    .chain(file.syntax.lex_errors.iter().filter_map(|err| {
      diagnostic(file, severities, err.range(), err.display(), err.code(), err.severity())
    }))
    .chain(file.syntax.parse.errors.iter().filter_map(|err| {
      diagnostic(file, severities, err.range(), err.display(), err.code(), err.severity())
    }))
    .chain(file.syntax.lower.errors.iter().filter_map(|err| {
      diagnostic(file, severities, err.range(), err.display(), err.code(), err.severity())
    }))
    .collect();
  let no_filter = matches!(options.filter, config::DiagnosticsFilter::None);
  let has_any_error = ret.iter().any(|x| matches!(x.severity, diagnostic_util::Severity::Error));
  if no_filter || !has_any_error {
    ret.extend(file.statics_errors.iter().filter_map(|err| {
      let idx = err.idx();
      let syntax = file.syntax.lower.ptrs.hir_to_ast(idx).expect("no pointer for idx");
      let node = syntax.to_node(file.syntax.parse.root.syntax());
      let range = custom_node_range(node.clone()).unwrap_or_else(|| node.text_range());
      let msg = err.display(syms, file.info.meta_vars(), options.lines);
      diagnostic(file, severities, range, msg, err.code(), err.severity())
    }));
    if options.format {
      if let Err(sml_fmt::Error::Comments(ranges)) = sml_fmt::check(&file.syntax.parse.root) {
        ret.extend(ranges.into_iter().filter_map(|range| {
          diagnostic(
            file,
            severities,
            range,
            "comment prevents formatting",
            diagnostic_util::Code::n(6001),
            diagnostic_util::Severity::Warning,
          )
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
    return Some(node.local_kw()?.text_range());
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
