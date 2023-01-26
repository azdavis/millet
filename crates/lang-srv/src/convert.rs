//! Conversions between LSP types and more commonly used types, like ones from analysis or the
//! standard library.

use anyhow::{bail, Context as _, Result};
use lsp_types::Url;
use std::fmt;

pub(crate) fn canonical_path_buf<F>(fs: &F, url: &Url) -> Result<paths::CanonicalPathBuf>
where
  F: paths::FileSystem,
{
  if url.scheme() != "file" {
    bail!("not a file url: {url}")
  }
  match url.to_file_path() {
    Ok(pb) => Ok(fs.canonicalize(pb.as_path())?),
    Err(()) => bail!("couldn't make a URL into a file path: {url}"),
  }
}

pub(crate) fn file_url(path: &std::path::Path) -> Result<Url> {
  Url::parse(&format!("file://{}", path.display()))
    .with_context(|| format!("couldn't parse path into a URL: {path:?}"))
}

pub(crate) fn diagnostics(
  errors: Vec<diagnostic_util::Diagnostic>,
  more_info_hint: bool,
) -> Vec<lsp_types::Diagnostic> {
  errors
    .into_iter()
    .map(|err| diagnostic(err.message, Some(err.range), err.code, err.severity, more_info_hint))
    .collect()
}

pub(crate) fn error_url(code: diagnostic_util::Code) -> Url {
  Url::parse(&format!("{}#{code}", analysis::URL)).expect("couldn't parse error URL")
}

struct ClickCodeHint {
  code: diagnostic_util::Code,
}

impl fmt::Display for ClickCodeHint {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "click the blue '{}' for more info and/or advice for how to fix. ", self.code)?;
    write!(f, "in VS Code, set `millet.server.diagnostics.moreInfoHint.enable` to `false` ")?;
    write!(f, "to disable this hint.")?;
    Ok(())
  }
}

pub(crate) fn diagnostic(
  message: String,
  range: Option<text_pos::RangeUtf16>,
  code: diagnostic_util::Code,
  severity: diagnostic_util::Severity,
  more_info_hint: bool,
) -> lsp_types::Diagnostic {
  let url = error_url(code);
  let related_information = more_info_hint.then(|| {
    vec![lsp_types::DiagnosticRelatedInformation {
      location: lsp_types::Location { uri: url.clone(), range: lsp_types::Range::default() },
      message: ClickCodeHint { code }.to_string(),
    }]
  });
  lsp_types::Diagnostic {
    range: range.map(lsp_range).unwrap_or_default(),
    severity: Some(match severity {
      diagnostic_util::Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
      diagnostic_util::Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
    }),
    code: Some(lsp_types::NumberOrString::Number(code.as_i32())),
    code_description: Some(lsp_types::CodeDescription { href: url }),
    source: Some("Millet".to_owned()),
    message,
    related_information,
    tags: None,
    data: None,
  }
}

pub(crate) fn lsp_range(range: text_pos::RangeUtf16) -> lsp_types::Range {
  lsp_types::Range { start: lsp_position(range.start), end: lsp_position(range.end) }
}

pub(crate) fn lsp_position(pos: text_pos::PositionUtf16) -> lsp_types::Position {
  lsp_types::Position { line: pos.line, character: pos.col }
}

pub(crate) fn lsp_location(
  store: &paths::Store,
  range: paths::WithPath<text_pos::RangeUtf16>,
) -> Option<lsp_types::Location> {
  let uri = match file_url(store.get_path(range.path).as_path()) {
    Ok(x) => x,
    Err(e) => {
      log::error!("couldn't get path as a file url: {e:#}");
      return None;
    }
  };
  Some(lsp_types::Location { uri, range: lsp_range(range.val) })
}

fn analysis_position(pos: lsp_types::Position) -> text_pos::PositionUtf16 {
  text_pos::PositionUtf16 { line: pos.line, col: pos.character }
}

pub(crate) fn analysis_range(range: lsp_types::Range) -> text_pos::RangeUtf16 {
  text_pos::RangeUtf16 { start: analysis_position(range.start), end: analysis_position(range.end) }
}

pub(crate) fn url_to_path_id<F>(
  fs: &F,
  store: &mut paths::Store,
  url: &Url,
) -> Result<paths::PathId>
where
  F: paths::FileSystem,
{
  Ok(store.get_id(&canonical_path_buf(fs, url)?))
}

pub(crate) fn text_doc_pos_params<F>(
  fs: &F,
  store: &mut paths::Store,
  params: &lsp_types::TextDocumentPositionParams,
) -> Result<paths::WithPath<text_pos::PositionUtf16>>
where
  F: paths::FileSystem,
{
  let path = url_to_path_id(fs, store, &params.text_document.uri)?;
  let pos = analysis_position(params.position);
  Ok(path.wrap(pos))
}

pub(crate) fn registration<N, T>(options: T) -> lsp_types::Registration
where
  N: lsp_types::notification::Notification,
  T: serde::Serialize,
{
  lsp_types::Registration {
    id: N::METHOD.to_owned(),
    method: N::METHOD.to_owned(),
    register_options: Some(serde_json::to_value(options).unwrap()),
  }
}

pub(crate) fn quick_fix(
  title: String,
  url: Url,
  range: text_pos::RangeUtf16,
  new_text: String,
) -> lsp_types::CodeActionOrCommand {
  lsp_types::CodeActionOrCommand::CodeAction(lsp_types::CodeAction {
    title,
    kind: Some(lsp_types::CodeActionKind::QUICKFIX),
    edit: Some(lsp_types::WorkspaceEdit {
      document_changes: Some(lsp_types::DocumentChanges::Edits(vec![
        lsp_types::TextDocumentEdit {
          text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
            uri: url,
            version: None,
          },
          edits: vec![lsp_types::OneOf::Left(lsp_types::TextEdit {
            range: lsp_range(range),
            new_text,
          })],
        },
      ])),
      ..Default::default()
    }),
    ..Default::default()
  })
}

pub(crate) fn document_symbol(sym: analysis::DocumentSymbol) -> lsp_types::DocumentSymbol {
  #[allow(deprecated)]
  lsp_types::DocumentSymbol {
    name: sym.name,
    detail: sym.detail,
    kind: match sym.kind {
      sml_namespace::SymbolKind::Signature => lsp_types::SymbolKind::INTERFACE,
      sml_namespace::SymbolKind::Structure => lsp_types::SymbolKind::MODULE,
      sml_namespace::SymbolKind::Functor | sml_namespace::SymbolKind::Function => {
        lsp_types::SymbolKind::FUNCTION
      }
      sml_namespace::SymbolKind::Value => lsp_types::SymbolKind::VARIABLE,
      sml_namespace::SymbolKind::Type => lsp_types::SymbolKind::CLASS,
      sml_namespace::SymbolKind::Constructor => lsp_types::SymbolKind::CONSTRUCTOR,
      sml_namespace::SymbolKind::Exception => lsp_types::SymbolKind::EVENT,
    },
    tags: None,
    deprecated: None,
    range: lsp_range(sym.range),
    selection_range: lsp_range(sym.selection_range),
    children: Some(sym.children.into_iter().map(document_symbol).collect()),
  }
}

pub(crate) fn completion_item(item: analysis::CompletionItem) -> lsp_types::CompletionItem {
  lsp_types::CompletionItem {
    label: item.label,
    kind: Some(match item.kind {
      sml_namespace::SymbolKind::Signature => lsp_types::CompletionItemKind::INTERFACE,
      sml_namespace::SymbolKind::Structure => lsp_types::CompletionItemKind::MODULE,
      sml_namespace::SymbolKind::Functor | sml_namespace::SymbolKind::Function => {
        lsp_types::CompletionItemKind::FUNCTION
      }
      sml_namespace::SymbolKind::Value => lsp_types::CompletionItemKind::VARIABLE,
      sml_namespace::SymbolKind::Type => lsp_types::CompletionItemKind::CLASS,
      sml_namespace::SymbolKind::Constructor => lsp_types::CompletionItemKind::CONSTRUCTOR,
      sml_namespace::SymbolKind::Exception => lsp_types::CompletionItemKind::EVENT,
    }),
    detail: item.detail,
    documentation: item.documentation.map(|value| {
      lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
        kind: lsp_types::MarkupKind::Markdown,
        value,
      })
    }),
    deprecated: None,
    preselect: None,
    sort_text: None,
    filter_text: None,
    insert_text: None,
    insert_text_format: None,
    insert_text_mode: None,
    text_edit: None,
    additional_text_edits: None,
    command: None,
    commit_characters: None,
    data: None,
    tags: None,
  }
}
