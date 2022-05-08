//! The core of the server logic.

use crate::comm::{
  IncomingNotification, IncomingRequestParams, Outgoing, OutgoingNotification, Request, Response,
  ResponseSuccess,
};
use base::intern::StrStoreMut;
use base::loc::Loc;
use base::{lex, parse, statics};
use lsp_types::{
  Diagnostic, InitializeResult, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
  ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
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
  pub fn handle_request(&mut self, req: Request<IncomingRequestParams>) -> Response {
    let res = match req.params {
      IncomingRequestParams::Initialize(params) => {
        // TODO do something with params.process_id
        self.root_uri = params.root_uri;
        let res = InitializeResult {
          capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            ..ServerCapabilities::default()
          },
          server_info: Some(ServerInfo {
            name: "millet-ls".to_owned(),
            version: Some(env!("CARGO_PKG_VERSION").to_owned()),
          }),
        };
        Ok(ResponseSuccess::Initialize(res.into()))
      }
      IncomingRequestParams::Shutdown => {
        self.got_shutdown = true;
        Ok(ResponseSuccess::Null)
      }
    };
    Response {
      id: Some(req.id),
      res,
    }
  }

  /// Handle a notification by possibly taking some action.
  pub fn handle_notification(&mut self, notif: IncomingNotification) -> Option<Action> {
    match notif {
      IncomingNotification::Initialized => None,
      IncomingNotification::Exit => Some(Action::Exit(self.got_shutdown)),
      IncomingNotification::TextDocOpen(params) => Some(mk_diagnostic_action(
        params.text_document.uri,
        Some(params.text_document.version),
        params.text_document.text.as_bytes(),
      )),
      IncomingNotification::TextDocChange(mut params) => {
        assert_eq!(params.content_changes.len(), 1);
        let change = params.content_changes.pop().unwrap();
        assert!(change.range.is_none());
        Some(mk_diagnostic_action(
          params.text_document.uri,
          Some(params.text_document.version),
          change.text.as_bytes(),
        ))
      }
      IncomingNotification::TextDocSave(_) => None,
      IncomingNotification::TextDocClose(_) => None,
    }
  }
}

/// An action to take in response to a notification.
pub enum Action {
  /// Exit the server. The bool is whether the process should exit successfully.
  Exit(bool),
  /// Respond with an outgoing message.
  Respond(Box<Outgoing>),
}

fn mk_diagnostic_action(uri: Url, version: Option<i32>, bs: &[u8]) -> Action {
  let diagnostics: Vec<_> = ck_one_file(bs).into_iter().collect();
  Action::Respond(
    Outgoing::Notification(OutgoingNotification::PublishDiagnostics(
      PublishDiagnosticsParams {
        uri,
        diagnostics,
        version,
      },
    ))
    .into(),
  )
}

fn ck_one_file(bs: &[u8]) -> Option<Diagnostic> {
  let mut store = StrStoreMut::new();
  let lexer = match lex::get(&mut store, bs) {
    Ok(x) => x,
    Err(e) => return Some(mk_diagnostic(bs, e.loc, e.val.message())),
  };
  let store = store.finish();
  let top_decs = match parse::get(lexer) {
    Ok(x) => x,
    Err(e) => return Some(mk_diagnostic(bs, e.loc, e.val.message(&store))),
  };
  let mut s = statics::Statics::new();
  for top_dec in top_decs {
    match s.get(&top_dec) {
      Ok(()) => {}
      Err(e) => return Some(mk_diagnostic(bs, e.loc, e.val.message(&store))),
    }
  }
  None
}

fn mk_diagnostic(bs: &[u8], loc: Loc, message: String) -> Diagnostic {
  let range: std::ops::Range<usize> = loc.into();
  let range = Range {
    start: position(bs, range.start),
    end: position(bs, range.end),
  };
  Diagnostic {
    range,
    message,
    source: Some("millet-ls".to_owned()),
    ..Diagnostic::default()
  }
}

fn position(bs: &[u8], byte_idx: usize) -> Position {
  let mut line = 0;
  let mut character = 0;
  for (idx, &b) in bs.iter().enumerate() {
    if idx == byte_idx {
      break;
    }
    if b == b'\n' {
      line += 1;
      character = 0;
    } else {
      character += 1;
    }
  }
  Position { line, character }
}
