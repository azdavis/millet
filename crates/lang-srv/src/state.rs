//! See [`State`].

#![allow(dead_code)]

use crossbeam_channel::Sender;
use lsp_server::{ExtractError, Message, Notification, ReqQueue, Request, RequestId};
use lsp_types::notification::Notification as _;
use std::ops::ControlFlow;
use walkdir::WalkDir;

/// The only thing that does file IO.
///
/// TODO: this is horribly inefficient.
pub(crate) struct State {
  root: lsp_types::Url,
  sender: Sender<Message>,
  req_queue: ReqQueue<(), ()>,
}

impl State {
  pub(crate) fn new(root: lsp_types::Url, sender: Sender<Message>) -> Self {
    let mut ret = Self {
      root,
      sender,
      req_queue: ReqQueue::default(),
    };
    ret.send_request::<lsp_types::request::RegisterCapability>(lsp_types::RegistrationParams {
      registrations: vec![lsp_types::Registration {
        id: lsp_types::notification::DidChangeWatchedFiles::METHOD.to_owned(),
        method: lsp_types::notification::DidChangeWatchedFiles::METHOD.to_owned(),
        register_options: Some(
          serde_json::to_value(lsp_types::DidChangeWatchedFilesRegistrationOptions {
            watchers: vec![lsp_types::FileSystemWatcher {
              glob_pattern: format!("{}/**/*.{{sml,sig,fun}}", ret.root.path()),
              kind: None,
            }],
          })
          .unwrap(),
        ),
      }],
    });
    ret.publish_diagnostics();
    ret
  }

  fn publish_diagnostics(&self) {
    let files = get_files(&self.root);
    let errors = analysis::get(files.iter().map(|(_, x)| x.as_str()), Default::default());
    for ((url, contents), errors) in files.into_iter().zip(errors) {
      let pos_db = text_pos::PositionDb::new(&contents);
      self.send_notification::<lsp_types::notification::PublishDiagnostics>(
        lsp_types::PublishDiagnosticsParams {
          uri: url,
          diagnostics: errors
            .into_iter()
            .map(|x| lsp_types::Diagnostic {
              range: lsp_range(pos_db.range(x.range)),
              message: x.message,
              source: Some("millet".to_owned()),
              ..lsp_types::Diagnostic::default()
            })
            .collect(),
          version: None,
        },
      );
    }
  }

  pub(crate) fn send_request<R>(&mut self, params: R::Params)
  where
    R: lsp_types::request::Request,
  {
    let req = self
      .req_queue
      .outgoing
      .register(R::METHOD.to_owned(), params, ());
    self.send(req.into())
  }

  pub(crate) fn send_response(&mut self, res: lsp_server::Response) {
    match self.req_queue.incoming.complete(res.id.clone()) {
      Some(()) => self.send(res.into()),
      None => eprintln!("tried to respond to a non-queued request: {res:?}"),
    }
  }

  pub(crate) fn send_notification<N>(&self, params: N::Params)
  where
    N: lsp_types::notification::Notification,
  {
    let notif = Notification::new(N::METHOD.to_owned(), params);
    self.send(notif.into())
  }

  pub(crate) fn handle_request(&mut self, req: lsp_server::Request) {
    eprintln!("got request: {req:?}");
    self.req_queue.incoming.register(req.id, ())
  }

  pub(crate) fn handle_response(&mut self, res: lsp_server::Response) {
    eprintln!("got response: {res:?}");
    match self.req_queue.outgoing.complete(res.id.clone()) {
      Some(()) => {}
      None => eprintln!("received response for non-queued request: {res:?}"),
    }
  }

  pub(crate) fn handle_notification(&self, notif: lsp_server::Notification) {
    eprintln!("got notification: {notif:?}");
    match extract_notification::<lsp_types::notification::DidChangeWatchedFiles>(notif) {
      ControlFlow::Break(_) => self.publish_diagnostics(),
      ControlFlow::Continue(_) => {}
    }
  }

  fn send(&self, msg: Message) {
    eprintln!("sending {msg:?}");
    self.sender.send(msg).unwrap()
  }
}

fn extract_request<R>(req: Request) -> ControlFlow<(RequestId, R::Params), ExtractError<Request>>
where
  R: lsp_types::request::Request,
{
  match req.extract(R::METHOD) {
    Ok(x) => ControlFlow::Break(x),
    Err(e) => ControlFlow::Continue(e),
  }
}

fn extract_notification<N>(
  notif: Notification,
) -> ControlFlow<N::Params, ExtractError<Notification>>
where
  N: lsp_types::notification::Notification,
{
  match notif.extract(N::METHOD) {
    Ok(x) => ControlFlow::Break(x),
    Err(e) => ControlFlow::Continue(e),
  }
}

/// Returns all the SML files in this workspace.
///
/// Ignores file IO errors, etc.
fn get_files(root: &lsp_types::Url) -> Vec<(lsp_types::Url, String)> {
  WalkDir::new(root.path())
    .into_iter()
    .filter_map(|entry| {
      let entry = entry.ok()?;
      let ext = entry.path().extension()?.to_str()?;
      let is_sml_ext = matches!(ext, "sml" | "sig" | "fun");
      if !is_sml_ext {
        return None;
      }
      let path = entry.path().as_os_str().to_str()?;
      // TODO is this right?
      let url = lsp_types::Url::parse(&format!("file://{path}")).ok()?;
      let contents = std::fs::read_to_string(entry.path()).ok()?;
      Some((url, contents))
    })
    .collect()
}

fn lsp_range(range: text_pos::Range) -> lsp_types::Range {
  lsp_types::Range {
    start: lsp_position(range.start),
    end: lsp_position(range.end),
  }
}

fn lsp_position(pos: text_pos::Position) -> lsp_types::Position {
  lsp_types::Position {
    line: pos.line,
    character: pos.character,
  }
}
