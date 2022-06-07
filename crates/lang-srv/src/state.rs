//! See [`State`].

#![allow(dead_code)]

use anyhow::{anyhow, Error};
use crossbeam_channel::Sender;
use lsp_server::{ExtractError, Message, Notification, ReqQueue, Request, RequestId};
use lsp_types::{notification::Notification as _, Url};
use std::ops::ControlFlow;
use walkdir::WalkDir;

const SOURCE: &str = "millet";

/// The only thing that does file IO.
///
/// TODO: this is horribly inefficient.
pub(crate) struct State {
  root: Url,
  sender: Sender<Message>,
  req_queue: ReqQueue<(), ()>,
}

impl State {
  pub(crate) fn new(root: Url, sender: Sender<Message>) -> Self {
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
              glob_pattern: format!("{}/**/*.{{sml,sig,fun,cm}}", ret.root.path()),
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
    let (ok_files, err_files) = get_files(&self.root);
    let errors = analysis::get(ok_files.iter().map(|(_, x)| x.as_str()), Default::default());
    for (url, error) in err_files {
      self.send_notification::<lsp_types::notification::PublishDiagnostics>(
        lsp_types::PublishDiagnosticsParams {
          uri: url,
          diagnostics: vec![lsp_types::Diagnostic {
            range: lsp_types::Range::default(),
            message: error.to_string(),
            source: Some(SOURCE.to_owned()),
            ..lsp_types::Diagnostic::default()
          }],
          version: None,
        },
      );
    }
    for ((url, contents), errors) in ok_files.into_iter().zip(errors) {
      let pos_db = text_pos::PositionDb::new(&contents);
      self.send_notification::<lsp_types::notification::PublishDiagnostics>(
        lsp_types::PublishDiagnosticsParams {
          uri: url,
          diagnostics: errors
            .into_iter()
            .map(|x| lsp_types::Diagnostic {
              range: lsp_range(pos_db.range(x.range)),
              message: x.message,
              source: Some(SOURCE.to_owned()),
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

type Files<T> = Vec<(Url, T)>;

/// Returns all the SML files referenced by SMLNJ CM files in this workspace.
///
/// Ignores file IO errors, etc.
///
/// NOTE: This uses CM files to discover SML files, but doesn't, in the slightest, implement any
/// cm features like privacy of exports. We just parse cm files to get the sml filenames.
fn get_files(root: &Url) -> (Files<String>, Files<Error>) {
  let mut ok = Files::<String>::new();
  let mut err = Files::<Error>::new();
  for entry in WalkDir::new(root.path()).sort_by_file_name() {
    let entry = match entry {
      Ok(x) => x,
      Err(_) => continue,
    };
    let path = entry.path();
    if path.extension().map_or(true, |x| x != "cm") {
      continue;
    }
    let url = match Url::parse(&format!("file://{}", path.display())) {
      Ok(x) => x,
      Err(_) => continue,
    };
    let contents = match std::fs::read_to_string(path) {
      Ok(x) => x,
      Err(e) => {
        err.push((url, e.into()));
        continue;
      }
    };
    let members = match cm::get(&contents) {
      Ok((_, x)) => x,
      Err(e) => {
        err.push((url, e));
        continue;
      }
    };
    let parent = match entry.path().parent() {
      Some(x) => x,
      None => {
        err.push((url, anyhow!("no parent")));
        continue;
      }
    };
    for path in members.into_iter() {
      let path = parent.join(&path);
      let url = match Url::parse(&format!("file://{}", path.display())) {
        Ok(x) => x,
        Err(_) => continue,
      };
      match std::fs::read_to_string(&path) {
        Ok(contents) => ok.push((url, contents)),
        Err(e) => err.push((url, e.into())),
      }
    }
  }
  (ok, err)
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
