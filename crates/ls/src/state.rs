//! The core of the server logic.

use crate::comm::{Notification, Request, RequestParams, Response, ResponseSuccess};
use lsp_types::{
  InitializeResult, ServerCapabilities, ServerInfo, TextDocumentSyncCapability,
  TextDocumentSyncKind, Url,
};
use std::collections::HashMap;

pub struct State {
  root_uri: Option<Url>,
  got_shutdown: bool,
  files: HashMap<Url, String>,
}

impl State {
  /// Returns a new State.
  pub fn new() -> Self {
    Self {
      root_uri: None,
      got_shutdown: false,
      files: HashMap::new(),
    }
  }

  /// Returns the Response for this Request.
  pub fn handle_req(&mut self, req: Request) -> Response {
    let res = match req.params {
      RequestParams::Initialize(params) => {
        // TODO do something with params.process_id
        self.root_uri = params.root_uri;
        Ok(ResponseSuccess::Initialize(InitializeResult {
          capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::Full)),
            ..ServerCapabilities::default()
          },
          server_info: Some(ServerInfo {
            name: "millet-ls".to_owned(),
            version: Some(env!("CARGO_PKG_VERSION").to_owned()),
          }),
        }))
      }
      RequestParams::Shutdown => {
        self.got_shutdown = true;
        Ok(ResponseSuccess::Null)
      }
    };
    Response {
      id: Some(req.id),
      res,
    }
  }

  /// Returns:
  /// - `None` if the server should continue running.
  /// - `Some(true)` if the server should exit successfully.
  /// - `Some(false)` if the server should exit with an error.
  pub fn handle_notif(&mut self, notif: Notification) -> Option<bool> {
    match notif {
      Notification::Initialized => {}
      Notification::Exit => return Some(self.got_shutdown),
      Notification::TextDocOpen(params) => {
        assert!(self
          .files
          .insert(params.text_document.uri, params.text_document.text)
          .is_none());
      }
      Notification::TextDocChange(mut params) => {
        assert_eq!(params.content_changes.len(), 1);
        let change = params.content_changes.pop().unwrap();
        assert!(self
          .files
          .insert(params.text_document.uri, change.text)
          .is_some());
      }
      Notification::TextDocSave(_) => {}
      Notification::TextDocClose(params) => {
        assert!(self.files.remove(&params.text_document.uri).is_some());
      }
    }
    None
  }
}
