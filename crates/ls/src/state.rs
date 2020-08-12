//! The core of the server logic.

use crate::comm::{
  IncomingNotification, IncomingRequestParams, Outgoing, OutgoingNotification, Request, Response,
  ResponseError, ResponseSuccess,
};
use lsp_types::{
  Diagnostic, DocumentSymbol, DocumentSymbolResponse, InitializeResult, Position,
  PublishDiagnosticsParams, Range, ServerCapabilities, ServerInfo, SymbolKind,
  TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

use millet_core::ast::{Dec, StrDec, TopDec};
use millet_core::intern::StrStoreMut;
use millet_core::loc::{Loc, Located};
use millet_core::{lex, parse, statics};

use std::fs::File;
use std::io::Read;

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
        Ok(ResponseSuccess::Initialize(InitializeResult {
          capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::Full)),
            document_symbol_provider: Some(true),
            ..ServerCapabilities::default()
          },
          server_info: Some(ServerInfo {
            name: "millet-ls".to_owned(),
            version: Some(env!("CARGO_PKG_VERSION").to_owned()),
          }),
        }))
      }
      IncomingRequestParams::Shutdown => {
        self.got_shutdown = true;
        Ok(ResponseSuccess::Null)
      }
      IncomingRequestParams::DocumentSymbols(params) => {
        let bs: Result<Vec<_>, _> = params
          .text_document
          .uri
          .to_file_path()
          .map_err(|_| {
            std::io::Error::new(
              std::io::ErrorKind::NotFound,
              "conversion of uri to path was unsuccessful",
            )
          })
          .and_then(File::open)
          .and_then(|f| f.bytes().collect());
        match bs {
          Ok(bs) => mk_document_symbols(params.text_document.uri, &bs[..]),
          Err(_) => Err(ResponseError {
            code: crate::comm::ErrorCode::ServerErrorStart,
            message: format!("Issue opening {} for reading", params.text_document.uri),
          }),
        }
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
          params.text_document.version,
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

fn mk_diagnostic_action(uri: Url, version: Option<i64>, bs: &[u8]) -> Action {
  let diagnostics: Vec<_> = ck_one_file(bs).into_iter().collect();
  Action::Respond(
    Outgoing::Notification(OutgoingNotification::PublishDiagnostics(
      PublishDiagnosticsParams {
        uri,
        version,
        diagnostics,
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

fn mk_document_symbols(uri: Url, bs: &[u8]) -> Result<ResponseSuccess, ResponseError> {
  let symbols: Vec<_> = outline_one_file(bs)?;
  Ok(ResponseSuccess::DocumentSymbol(
    DocumentSymbolResponse::Nested(symbols),
  ))
}

fn outline_one_file(bs: &[u8]) -> Result<Vec<DocumentSymbol>, ResponseError> {
  let mut store = StrStoreMut::new();
  let lexer = match lex::get(&mut store, bs) {
    Ok(x) => x,
    Err(_) => {
      return Err(ResponseError {
        code: crate::comm::ErrorCode::ServerErrorStart, // TODO: better error
        message: "encountered a lexer error".to_owned(),
      });
    }
  };
  let store = store.finish();
  let mut top_decs = match parse::get(lexer) {
    Ok(x) => x,
    Err(_) => {
      return Err(ResponseError {
        code: crate::comm::ErrorCode::ServerErrorStart, // TODO ibidem
        message: "encountered a parser error".to_owned(),
      });
    }
  };
  let ndecs = top_decs.len();
  Ok(
    top_decs
      .drain(..)
      // TODO: move to new function instead of closure
      .fold(Vec::with_capacity(ndecs), |mut symbs, dec| {
        match dec.val {
          TopDec::StrDec(Located { loc: _, val }) => match val {
            StrDec::Dec(Located { loc, val }) => match val {
              Dec::Val(_, binding) => {
                symbs.push(DocumentSymbol {
                  name: String::from_utf8_lossy(&bs[std::ops::Range::from(binding[0].pat.loc)])
                    .into(),
                  detail: None,
                  kind: SymbolKind::Variable,
                  deprecated: None,
                  range: range_from_loc(bs, loc),
                  selection_range: range_from_loc(bs, binding[0].pat.loc),
                  children: None,
                });
                symbs
              }
              Dec::Fun(_, binding) => {
                symbs.push(DocumentSymbol {
                  name: String::from_utf8_lossy(
                    // Maybe implement SliceIndex for Loc?
                    &bs[std::ops::Range::from(binding[0].cases[0].vid.loc)],
                  )
                  .into(),
                  detail: Some("'a -> 'a".to_owned()),
                  kind: SymbolKind::Function,
                  deprecated: None,
                  range: range_from_loc(bs, loc),
                  selection_range: range_from_loc(bs, binding[0].cases[0].vid.loc),
                  children: None,
                });
                symbs
              }
              _ => todo!("strdec dec non-val/fun"),
            },
            StrDec::Structure(..) => todo!("strdec structure"),
            StrDec::Local(..) => todo!("strdec local"),
            StrDec::Seq(..) => todo!("strdec seq"),
          },
          TopDec::SigDec(..) => todo!("sigdec"),
          TopDec::FunDec(..) => todo!("fundec"),
        }
      }),
  )
}

fn range_from_loc(bs: &[u8], loc: Loc) -> Range {
  let range: std::ops::Range<usize> = loc.into();
  Range {
    start: position(bs, range.start),
    end: position(bs, range.end),
  }
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
