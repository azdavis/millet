//! The core of the server logic.

use std::collections::BTreeMap;
use std::io::{self, Read};
use std::path::PathBuf;

use crate::comm::{
  IncomingNotification, IncomingRequestParams, Outgoing, OutgoingNotification, Request, Response,
  ResponseSuccess,
};
use crate::workspace::ProjectWorkspace;

use lsp_types::{
  Diagnostic, InitializeResult, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
  ServerInfo, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

use millet_core::intern::StrStoreMut;
use millet_core::loc::Loc;
use millet_core::{lex, parse, statics};

#[derive(Debug)]
pub struct State {
  root_uri: Option<Url>,
  got_shutdown: bool,
  workspace: ProjectWorkspace,
  // TODO: If this is just cache need separate thing for deps
  // (idk if just want to store in config? :/)
  cache: Cache,
}

impl State {
  /// Returns a new State.
  pub fn new() -> Self {
    Self {
      root_uri: None,
      got_shutdown: false,
      workspace: Default::default(),
      cache: BTreeMap::new(),
    }
  }

  /// Returns the Response for this Request.
  pub fn handle_request(&mut self, req: Request<IncomingRequestParams>) -> Response {
    let res = match req.params {
      IncomingRequestParams::Initialize(params) => {
        // TODO do something with params.process_id
        self.root_uri = params.root_uri;
        if let Some(ref root) = self.root_uri {
          if let Ok(c) = ProjectWorkspace::new(root) {
            self.workspace = c;
            self.cache = init_cache(self.workspace.get_files());
          }
        }
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
        self.workspace.get_files(),
        &self.cache,
      )),
      IncomingNotification::TextDocChange(mut params) => {
        assert_eq!(params.content_changes.len(), 1);
        let change = params.content_changes.pop().unwrap();
        assert!(change.range.is_none());
        self
          .cache
          .entry(params.text_document.uri.clone())
          .and_modify(|e| e.update(change.text.as_bytes().into()));
        Some(mk_diagnostic_action(
          params.text_document.uri,
          params.text_document.version,
          self.workspace.get_files(),
          &self.cache,
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

fn mk_diagnostic_action(
  uri: Url,
  version: Option<i64>,
  files: &'_ [Url],
  cache: &'_ Cache,
) -> Action {
  let (uri, diagnostics) = match ck_files(files, cache) {
    Some((uri, diag)) => (uri, vec![diag]),
    None => (uri, vec![]),
  };
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

fn ck_files(paths: &'_ [Url], cache: &'_ Cache) -> Option<(Url, Diagnostic)> {
  let files: Vec<_> = paths
    .into_iter()
    .map(|path| cache.get(path).map(|sf| &sf.bytes))
    .collect::<Option<_>>()?;
  let mut store = StrStoreMut::new();
  let lexers: Vec<_> = match files
    .iter()
    .enumerate()
    .map(|(i, bs)| match lex::get(&mut store, bs) {
      Ok(x) => Ok(x),
      Err(e) => Err(Some((
        paths[i].clone(),
        mk_diagnostic(bs, e.loc, e.val.message()),
      ))),
    })
    .collect()
  {
    Ok(x) => x,
    Err(e) => return e,
  };
  let store = store.finish();
  let file_top_decs: Vec<_> = match lexers
    .into_iter()
    .enumerate()
    .map(|(i, lexer)| match parse::get(lexer) {
      Ok(x) => Ok((i, x)),
      Err(e) => Err(Some((
        paths[i].clone(),
        mk_diagnostic(&files[i], e.loc, e.val.message(&store)),
      ))),
    })
    .collect()
  {
    Ok(x) => x,
    Err(e) => return e,
  };
  let mut s = statics::Statics::new();
  for (file, top_decs) in file_top_decs {
    for top_dec in top_decs {
      match s.get(&top_dec) {
        Ok(()) => {}
        Err(e) => {
          return Some((
            paths[file].clone(),
            mk_diagnostic(&files[file], e.loc, e.val.message(&store)),
          ))
        }
      }
    }
  }
  None
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

type Cache = BTreeMap<Url, SourceFile>;

fn init_cache(uris: &[Url]) -> Cache {
  uris
    .iter()
    .filter_map(|uri| match SourceFile::new(uri.clone()).ok() {
      Some(sf) => Some((uri.clone(), sf)),
      None => None,
    })
    .collect()
}

#[derive(Debug)]
struct SourceFile {
  path: PathBuf,
  bytes: Vec<u8>,
}

impl SourceFile {
  fn new(uri: Url) -> io::Result<SourceFile> {
    let path = uri
      .to_file_path()
      .map_err(|_| io::ErrorKind::InvalidInput)?;
    std::fs::File::open(&path).map(|mut f| {
      let mut bytes = Vec::new();
      f.read_to_end(&mut bytes)
        .and_then(|_| Ok(SourceFile { path, bytes }))
    })?
  }

  fn update(&mut self, bs: Vec<u8>) {
    self.bytes = bs
  }
}
