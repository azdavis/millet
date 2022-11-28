//! Helpers, often for converting from "Millet" types to "language server protocol" types.

use anyhow::{anyhow, bail, Context as _, Result};
use lsp_server::{ExtractError, Notification, Request, RequestId};
use lsp_types::Url;
use std::fmt;
use std::ops::ControlFlow;

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
  Url::parse(&format!("{}#{}", diagnostic_util::URL, code)).expect("couldn't parse error URL")
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
  range: Option<text_pos::Range>,
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

pub(crate) fn lsp_range(range: text_pos::Range) -> lsp_types::Range {
  lsp_types::Range { start: lsp_position(range.start), end: lsp_position(range.end) }
}

pub(crate) fn lsp_position(pos: text_pos::Position) -> lsp_types::Position {
  lsp_types::Position { line: pos.line, character: pos.character }
}

pub(crate) fn lsp_location(
  store: &paths::Store,
  range: paths::WithPath<text_pos::Range>,
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

fn analysis_position(pos: lsp_types::Position) -> text_pos::Position {
  text_pos::Position { line: pos.line, character: pos.character }
}

pub(crate) fn analysis_range(range: lsp_types::Range) -> text_pos::Range {
  text_pos::Range { start: analysis_position(range.start), end: analysis_position(range.end) }
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
  params: lsp_types::TextDocumentPositionParams,
) -> Result<paths::WithPath<text_pos::Position>>
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
  range: text_pos::Range,
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

pub(crate) fn try_req<R, F>(req: Request, f: F) -> ControlFlow<Result<()>, Request>
where
  R: lsp_types::request::Request,
  F: FnOnce(RequestId, R::Params) -> Result<()>,
{
  match req.extract::<R::Params>(R::METHOD) {
    Ok((id, params)) => ControlFlow::Break(f(id, params)),
    Err(e) => extract_error(e),
  }
}

pub(crate) fn try_notif<N, F>(notif: Notification, f: F) -> ControlFlow<Result<()>, Notification>
where
  N: lsp_types::notification::Notification,
  F: FnOnce(N::Params) -> Result<()>,
{
  match notif.extract::<N::Params>(N::METHOD) {
    Ok(params) => ControlFlow::Break(f(params)),
    Err(e) => extract_error(e),
  }
}

fn extract_error<T>(e: ExtractError<T>) -> ControlFlow<Result<()>, T> {
  match e {
    ExtractError::MethodMismatch(x) => ControlFlow::Continue(x),
    ExtractError::JsonError { method, error } => {
      ControlFlow::Break(Err(anyhow!("couldn't deserialize for {method}: {error}")))
    }
  }
}

/// adapted from rust-analyzer.
pub(crate) fn apply_changes(
  text: &mut String,
  changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
) {
  let mut pos_db = text_pos::PositionDb::new(text);
  let mut up_to_line = None::<u32>;
  for change in changes {
    match change.range {
      Some(range) => {
        if up_to_line.map_or(false, |utl| utl <= range.end.line) {
          pos_db = text_pos::PositionDb::new(text);
        }
        match pos_db.text_range(analysis_range(range)) {
          Some(text_range) => {
            text.replace_range(std::ops::Range::<usize>::from(text_range), &change.text);
            up_to_line = Some(range.start.line);
          }
          None => log::error!("unable to apply text document change {change:?}"),
        }
      }
      None => {
        *text = change.text;
        up_to_line = Some(0);
      }
    }
  }
}
