//! The core of the server logic.

use crate::comm::{Notification, Request, RequestParams, Response, ResponseSuccess};
use lsp_types::{
  InitializeResult, ServerCapabilities, ServerInfo, TextDocumentSyncCapability,
  TextDocumentSyncKind, Url,
};

pub struct State {
  root_uri: Option<Url>,
}

impl State {
  /// Returns a new State.
  pub fn new() -> Self {
    Self { root_uri: None }
  }

  /// Returns the Response for this Request.
  pub fn handle_req(&mut self, req: Request) -> Response {
    let res = match req.params {
      RequestParams::Initialize(params) => {
        // let _ = params.process_id?;
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
      RequestParams::Shutdown => Ok(ResponseSuccess::Null),
    };
    Response {
      id: Some(req.id),
      res,
    }
  }

  /// Returns whether the server should continue running.
  pub fn handle_notif(&mut self, notif: Notification) -> bool {
    match notif {
      Notification::Initialized => true,
      Notification::Exit => false,
    }
  }
}
