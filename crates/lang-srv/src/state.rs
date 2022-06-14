//! See [`State`].

use anyhow::{anyhow, Error, Result};
use crossbeam_channel::Sender;
use fast_hash::FxHashSet;
use lsp_server::{ExtractError, Message, Notification, ReqQueue, Request, RequestId};
use lsp_types::{notification::Notification as _, Url};
use std::ops::ControlFlow;
use walkdir::WalkDir;

pub(crate) fn capabilities() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    ..Default::default()
  }
}

const SOURCE: &str = "millet";
const MAX_FILES_WITH_ERRORS: usize = 20;
const MAX_ERRORS_PER_FILE: usize = 20;

/// The state of the language server. Only this may do IO. (Well, also the [`lsp_server`] channels
/// that communicate over stdin and stdout.)
///
/// TODO: this is horribly inefficient.
pub(crate) struct State {
  root: Url,
  sender: Sender<Message>,
  req_queue: ReqQueue<(), ()>,
  has_diagnostics: FxHashSet<Url>,
  analysis: analysis::Analysis,
}

impl State {
  pub(crate) fn new(root: Url, sender: Sender<Message>) -> Self {
    let mut ret = Self {
      root,
      sender,
      req_queue: ReqQueue::default(),
      has_diagnostics: FxHashSet::default(),
      analysis: analysis::Analysis::default(),
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
    let (ok_files, err_files) = get_files(&ret.root);
    ret.publish_diagnostics(ok_files, err_files);
    ret
  }

  fn publish_diagnostics(
    &mut self,
    ok_files: Files<String>,
    err_files: Files<Error>,
  ) -> FxHashSet<Url> {
    let mut has_diagnostics = FxHashSet::<Url>::default();
    let analysis_errors = self.analysis.get(ok_files.iter().map(|(_, x)| x.as_str()));
    let file_errors = err_files
      .into_iter()
      .map(|(url, error)| (url, vec![(lsp_types::Range::default(), error.to_string())]));
    let analysis_errors =
      ok_files
        .into_iter()
        .zip(analysis_errors)
        .map(|((url, contents), errors)| {
          let pos_db = text_pos::PositionDb::new(&contents);
          let errors: Vec<_> = errors
            .into_iter()
            .map(|e| (lsp_range(pos_db.range(e.range)), e.message))
            .take(MAX_ERRORS_PER_FILE)
            .collect();
          (url, errors)
        });
    let errors = std::iter::empty()
      .chain(file_errors)
      .chain(analysis_errors)
      .take(MAX_FILES_WITH_ERRORS);
    for (url, errors) in errors {
      has_diagnostics.insert(url.clone());
      self.send_notification::<lsp_types::notification::PublishDiagnostics>(
        lsp_types::PublishDiagnosticsParams {
          uri: url,
          diagnostics: errors
            .into_iter()
            .map(|(range, message)| lsp_types::Diagnostic {
              range,
              message,
              source: Some(SOURCE.to_owned()),
              ..Default::default()
            })
            .collect(),
          version: None,
        },
      );
    }
    has_diagnostics
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

  #[allow(dead_code)]
  pub(crate) fn send_response(&mut self, res: lsp_server::Response) {
    match self.req_queue.incoming.complete(res.id.clone()) {
      Some(()) => self.send(res.into()),
      None => log::warn!("tried to respond to a non-queued request: {res:?}"),
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
    log::debug!("got request: {req:?}");
    self.req_queue.incoming.register(req.id, ())
  }

  pub(crate) fn handle_response(&mut self, res: lsp_server::Response) {
    log::debug!("got response: {res:?}");
    match self.req_queue.outgoing.complete(res.id.clone()) {
      Some(()) => {}
      None => log::warn!("received response for non-queued request: {res:?}"),
    }
  }

  pub(crate) fn handle_notification(&mut self, notif: lsp_server::Notification) {
    log::debug!("got notification: {notif:?}");
    match self.handle_notification_(notif) {
      ControlFlow::Break(Ok(())) => {}
      ControlFlow::Break(Err(e)) => log::error!("couldn't handle notification: {e}"),
      ControlFlow::Continue(notif) => log::warn!("unhandled notification: {notif:?}"),
    }
  }

  fn handle_notification_(
    &mut self,
    mut n: lsp_server::Notification,
  ) -> ControlFlow<Result<()>, Notification> {
    n = try_notification::<lsp_types::notification::DidChangeWatchedFiles, _>(n, |_| {
      let (ok_files, err_files) = get_files(&self.root);
      let mut has_diagnostics = self.publish_diagnostics(ok_files, err_files);
      std::mem::swap(&mut has_diagnostics, &mut self.has_diagnostics);
      // this is now the _old_ has_diagnostics.
      for url in has_diagnostics {
        if self.has_diagnostics.contains(&url) {
          continue;
        }
        // did used to have diagnostics, now don't. clear the diagnostics.
        self.send_notification::<lsp_types::notification::PublishDiagnostics>(
          lsp_types::PublishDiagnosticsParams {
            uri: url,
            diagnostics: Vec::new(),
            version: None,
          },
        );
      }
    })?;
    ControlFlow::Continue(n)
  }

  fn send(&self, msg: Message) {
    log::debug!("sending {msg:?}");
    self.sender.send(msg).unwrap()
  }
}

#[allow(dead_code)]
fn try_request<R, F>(req: Request, f: F) -> ControlFlow<Result<()>, Request>
where
  R: lsp_types::request::Request,
  F: FnOnce(RequestId, R::Params),
{
  match req.extract::<R::Params>(R::METHOD) {
    Ok((id, params)) => {
      f(id, params);
      ControlFlow::Break(Ok(()))
    }
    Err(e) => match e {
      ExtractError::MethodMismatch(req) => ControlFlow::Continue(req),
      ExtractError::JsonError { method, error } => {
        ControlFlow::Break(Err(anyhow!("couldn't deserialize for {method}: {error}")))
      }
    },
  }
}

fn try_notification<N, F>(notif: Notification, f: F) -> ControlFlow<Result<()>, Notification>
where
  N: lsp_types::notification::Notification,
  F: FnOnce(N::Params),
{
  match notif.extract::<N::Params>(N::METHOD) {
    Ok(params) => {
      f(params);
      ControlFlow::Break(Ok(()))
    }
    Err(e) => match e {
      ExtractError::MethodMismatch(notif) => ControlFlow::Continue(notif),
      ExtractError::JsonError { method, error } => {
        ControlFlow::Break(Err(anyhow!("could not deserialize for {method}: {error}")))
      }
    },
  }
}

type Files<T> = Vec<(Url, T)>;

/// Returns all the SML files referenced by SMLNJ CM files in this workspace.
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
