//! The core of the server logic.

use crate::comm::{
  IncomingNotification, Outgoing, Request, RequestParams, Response, ResponseSuccess,
};
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
  pub fn handle_request(&mut self, req: Request) -> Response {
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

  pub fn handle_notification(&mut self, notif: IncomingNotification) -> NotificationAction {
    match notif {
      IncomingNotification::Initialized => NotificationAction::Nothing,
      IncomingNotification::Exit => NotificationAction::Exit(self.got_shutdown),
      IncomingNotification::TextDocOpen(params) => {
        assert!(self
          .files
          .insert(params.text_document.uri, params.text_document.text)
          .is_none());
        NotificationAction::Nothing
      }
      IncomingNotification::TextDocChange(mut params) => {
        assert_eq!(params.content_changes.len(), 1);
        let change = params.content_changes.pop().unwrap();
        assert!(self
          .files
          .insert(params.text_document.uri, change.text)
          .is_some());
        NotificationAction::Nothing
      }
      IncomingNotification::TextDocSave(_) => NotificationAction::Nothing,
      IncomingNotification::TextDocClose(params) => {
        assert!(self.files.remove(&params.text_document.uri).is_some());
        NotificationAction::Nothing
      }
    }
  }
}

/// An action to take in response to a notification.
pub enum NotificationAction {
  /// Do nothing.
  Nothing,
  /// Exit the server. The bool is whether the process should exit cleanly.
  Exit(bool),
  /// Respond with an outgoing message.
  Respond(Outgoing),
}
