//! See [`State`].

use anyhow::{anyhow, bail, Error, Result};
use crossbeam_channel::Sender;
use fast_hash::FxHashSet;
use lsp_server::{ExtractError, Message, Notification, ReqQueue, Request, RequestId};
use lsp_types::{notification::Notification as _, Url};
use std::ops::ControlFlow;
use walkdir::WalkDir;

pub(crate) fn capabilities() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
      lsp_types::TextDocumentSyncOptions {
        open_close: Some(true),
        save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(
          lsp_types::SaveOptions {
            include_text: Some(true),
          },
        )),
        ..Default::default()
      },
    )),
    ..Default::default()
  }
}

const SOURCE: &str = "millet";
const MAX_FILES_WITH_ERRORS: usize = 20;
const MAX_ERRORS_PER_FILE: usize = 20;

struct Root {
  path: paths::Root,
  has_diagnostics: FxHashSet<Url>,
}

/// The state of the language server. Only this may do IO. (Well, also the [`lsp_server`] channels
/// that communicate over stdin and stdout.)
///
/// TODO: this is horribly inefficient.
pub(crate) struct State {
  root: Option<Root>,
  sender: Sender<Message>,
  req_queue: ReqQueue<(), ()>,
  analysis: analysis::Analysis,
}

impl State {
  pub(crate) fn new(root: Option<Url>, sender: Sender<Message>) -> Self {
    let mut ret = Self {
      // TODO report errors for path buf failure
      root: root
        .and_then(|x| canonical_path_buf(x).ok())
        .map(|root_path| Root {
          path: paths::Root::new(root_path),
          has_diagnostics: FxHashSet::default(),
        }),
      sender,
      req_queue: ReqQueue::default(),
      analysis: analysis::Analysis::default(),
    };
    if let Some(root) = &ret.root {
      let glob_pattern = format!("{}/**/*.{{sml,sig,fun,cm}}", root.path.as_path().display());
      // ... then end the borrow and mutate `ret`, to satisfy the borrow checker.
      ret.send_request::<lsp_types::request::RegisterCapability>(lsp_types::RegistrationParams {
        registrations: vec![lsp_types::Registration {
          id: lsp_types::notification::DidChangeWatchedFiles::METHOD.to_owned(),
          method: lsp_types::notification::DidChangeWatchedFiles::METHOD.to_owned(),
          register_options: Some(
            serde_json::to_value(lsp_types::DidChangeWatchedFilesRegistrationOptions {
              watchers: vec![lsp_types::FileSystemWatcher {
                glob_pattern,
                kind: None,
              }],
            })
            .unwrap(),
          ),
        }],
      });
      ret.publish_diagnostics();
    }
    ret
  }

  fn publish_diagnostics(&mut self) -> bool {
    let mut root = match self.root.take() {
      Some(x) => x,
      None => return false,
    };
    let (ok_files, err_files) = get_files(root.path.as_path());
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
      self.send_diagnostics(
        url,
        errors
          .into_iter()
          .map(|(range, message)| lsp_types::Diagnostic {
            range,
            message,
            source: Some(SOURCE.to_owned()),
            ..Default::default()
          })
          .collect(),
      );
    }
    // this is the old one.
    for url in root.has_diagnostics {
      // this is the new one.
      if has_diagnostics.contains(&url) {
        // had old diagnostics, and has new diagnostics. we just sent the new ones.
        continue;
      }
      // had old diagnostics, but no new diagnostics. clear the old diagnostics.
      self.send_diagnostics(url, Vec::new());
    }
    root.has_diagnostics = has_diagnostics;
    self.root = Some(root);
    true
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
      self.publish_diagnostics();
    })?;
    n = try_notification::<lsp_types::notification::DidOpenTextDocument, _>(n, |params| {
      if self.root.is_some() {
        return;
      }
      let url = params.text_document.uri;
      let text = params.text_document.text;
      self.publish_diagnostics_one(url, &text)
    })?;
    n = try_notification::<lsp_types::notification::DidSaveTextDocument, _>(n, |params| {
      if self.root.is_some() {
        return;
      }
      let url = params.text_document.uri;
      if let Some(text) = params.text {
        self.publish_diagnostics_one(url, &text)
      }
    })?;
    n = try_notification::<lsp_types::notification::DidCloseTextDocument, _>(n, |params| {
      if self.root.is_some() {
        return;
      }
      let url = params.text_document.uri;
      self.send_diagnostics(url, Vec::new());
    })?;
    ControlFlow::Continue(n)
  }

  fn publish_diagnostics_one(&mut self, url: Url, text: &str) {
    let pos_db = text_pos::PositionDb::new(text);
    self.send_diagnostics(url, diagnostics(&pos_db, self.analysis.get_one(text)));
  }

  fn send_diagnostics(&mut self, url: Url, diagnostics: Vec<lsp_types::Diagnostic>) {
    self.send_notification::<lsp_types::notification::PublishDiagnostics>(
      lsp_types::PublishDiagnosticsParams {
        uri: url,
        diagnostics,
        version: None,
      },
    );
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
    Err(e) => extract_error(e),
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
    Err(e) => extract_error(e),
  }
}

fn extract_error<T>(e: ExtractError<T>) -> ControlFlow<Result<()>, T> {
  match e {
    ExtractError::MethodMismatch(x) => ControlFlow::Continue(x),
    ExtractError::JsonError { method, error } => {
      ControlFlow::Break(Err(anyhow!("couldn't deserialize for {method}: {error}")))
    }
  }
}

type Files<T> = Vec<(Url, T)>;

/// Returns all the SML files referenced by SMLNJ CM files in this workspace.
///
/// NOTE: This uses CM files to discover SML files, but doesn't, in the slightest, implement any
/// cm features like privacy of exports. We just parse cm files to get the sml filenames.
fn get_files(root: &std::path::Path) -> (Files<String>, Files<Error>) {
  let mut ok = Files::<String>::new();
  let mut err = Files::<Error>::new();
  for entry in WalkDir::new(root).sort_by_file_name() {
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
      Ok(file) => file.sml,
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

fn canonical_path_buf(url: Url) -> Result<paths::CanonicalPathBuf> {
  if url.scheme() != "file" {
    bail!("not a file url")
  }
  match url.to_file_path() {
    Ok(pb) => Ok(pb.as_path().try_into()?),
    Err(()) => bail!("invalid url"),
  }
}

fn diagnostics(
  pos_db: &text_pos::PositionDb,
  errors: Vec<analysis::Error>,
) -> Vec<lsp_types::Diagnostic> {
  errors
    .into_iter()
    .take(MAX_ERRORS_PER_FILE)
    .map(|x| lsp_types::Diagnostic {
      range: lsp_range(pos_db.range(x.range)),
      message: x.message,
      source: Some(SOURCE.to_owned()),
      ..Default::default()
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
