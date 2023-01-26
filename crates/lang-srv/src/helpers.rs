//! Misc. helpers.

use crate::convert::analysis_range;
use anyhow::{anyhow, Result};
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
  text: &mut String,
  changes: Vec<lsp_types::TextDocumentContentChangeEvent>,
) {
  let mut pos_db = text_pos::PositionDb::new(text);
  let mut up_to_line = u32::MAX;
  for change in changes {
    match change.range {
      Some(range) => {
        if up_to_line <= range.end.line {
          pos_db = text_pos::PositionDb::new(text);
        }
        match pos_db.text_range_utf16(analysis_range(range)) {
          Some(text_range) => {
            text.replace_range(std::ops::Range::<usize>::from(text_range), &change.text);
            up_to_line = range.start.line;
          }
          None => log::error!("unable to apply text document change {change:?}"),
        }
      }
      None => {
        *text = change.text;
        up_to_line = 0;
      }
    }
  }
}
