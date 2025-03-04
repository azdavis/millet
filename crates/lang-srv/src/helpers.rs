//! Misc. helpers.

use crate::convert::analysis_range;
use anyhow::{Result, anyhow};
use lsp_server::{ExtractError, Notification, Request, RequestId};
use std::ops::ControlFlow;

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
  contents: &mut String,
  mut content_changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
) {
  // If at least one of the changes is a full document change, use the last of them as the starting
  // point and ignore all previous changes.
  let content_changes = match content_changes.iter().rposition(|change| change.range.is_none()) {
    Some(idx) => {
      *contents = std::mem::take(&mut content_changes[idx].text);
      &content_changes[idx + 1..]
    }
    None => &content_changes[..],
  };
  if content_changes.is_empty() {
    return;
  }

  let mut pos_db = text_pos::PositionDb::new(contents);

  // The changes we got must be applied sequentially, but can cross lines so we have to keep our
  // line index updated. Some clients (e.g. Code) sort the ranges in reverse. As an optimization, we
  // remember the last valid line in the index and only rebuild it if needed. The VFS will normalize
  // the end of lines to `\n`.
  let mut index_valid = u32::MAX;
  for change in content_changes {
    // The None case can't happen as we have handled it above already
    let Some(range) = change.range else { continue };
    if index_valid <= range.end.line {
      pos_db = text_pos::PositionDb::new(contents);
    }
    index_valid = range.start.line;
    if let Some(range) = pos_db.text_range_utf16(analysis_range(range)) {
      contents.replace_range(std::ops::Range::<usize>::from(range), &change.text);
    }
  }
}
