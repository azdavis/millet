//! The core of the server logic.

use crate::comm::{Notification, Request, RequestParams, Response, ResponseSuccess};
use lsp_types::{
  InitializeResult, ServerCapabilities, ServerInfo, TextDocumentSyncCapability,
  TextDocumentSyncKind, Url,
};

pub struct State {
  root_uri: Option<Url>,
  got_shutdown: bool,
}

impl State {
  /// Returns a new State.
  pub fn new() -> Self {
    Self {
      root_uri: None,
      got_shutdown: false,
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
  pub fn handle_notif(&self, notif: Notification) -> Option<bool> {
    match notif {
      Notification::Initialized => None,
      Notification::Exit => Some(self.got_shutdown),
    }
  }
}
