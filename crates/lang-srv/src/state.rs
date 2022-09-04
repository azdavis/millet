//! See [`State`].
//!
//! TODO progress indicator?

use anyhow::{anyhow, bail, Context, Result};
use crossbeam_channel::Sender;
use fast_hash::FxHashSet;
use lsp_server::{ExtractError, Message, Notification, ReqQueue, Request, RequestId, Response};
use lsp_types::Url;
use std::ops::ControlFlow;

pub(crate) fn capabilities() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
    definition_provider: Some(lsp_types::OneOf::Left(true)),
    type_definition_provider: Some(lsp_types::TypeDefinitionProviderCapability::Simple(true)),
    code_action_provider: Some(lsp_types::CodeActionProviderCapability::Simple(true)),
    ..Default::default()
  }
}

const MAX_FILES_WITH_ERRORS: usize = 20;

struct Root {
  input: analysis::input::Root,
  has_diagnostics: FxHashSet<Url>,
}

/// The state of the language server. Only this may do IO. (Well, also the [`lsp_server`] channels
/// that communicate over stdin and stdout.)
pub(crate) struct State {
  root: Option<Root>,
  sender: Sender<Message>,
  req_queue: ReqQueue<(), ()>,
  analysis: analysis::Analysis,
  file_system: paths::RealFileSystem,
  options: config::Options,
}

impl State {
  pub(crate) fn new(root: Option<Url>, options: config::Options, sender: Sender<Message>) -> Self {
    let file_system = paths::RealFileSystem::default();
    let mut root = root
      .map(|url| canonical_path_buf(&file_system, &url))
      .transpose();
    let mut ret = Self {
      // do this convoluted incantation because we need `ret` to `show_error` in the `Err` case.
      root: root
        .as_mut()
        .ok()
        .and_then(Option::take)
        .map(|root_path| Root {
          input: analysis::input::get_root_dir(root_path),
          has_diagnostics: FxHashSet::default(),
        }),
      sender,
      req_queue: ReqQueue::default(),
      analysis: analysis::Analysis::new(analysis::StdBasis::full(), config::ErrorLines::Many),
      file_system,
      options,
    };
    if let Err(e) = root {
      ret.show_error(format!("{e:#}"));
    }
    let registrations = match ret.root.take() {
      Some(root) => {
        let watchers = vec![lsp_types::FileSystemWatcher {
          // not sure if possible to only listen to millet.toml. "nested alternate groups are not
          // allowed" at time of writing
          glob_pattern: format!(
            "{}/**/*.{{sml,sig,fun,cm,mlb,toml}}",
            root.input.as_paths().as_path().display()
          ),
          kind: None,
        }];
        ret.publish_diagnostics(root);
        let did_change_watched = registration::<lsp_types::notification::DidChangeWatchedFiles, _>(
          lsp_types::DidChangeWatchedFilesRegistrationOptions { watchers },
        );
        vec![did_change_watched]
      }
      None => {
        let tdr = lsp_types::TextDocumentRegistrationOptions::default();
        vec![
          registration::<lsp_types::notification::DidOpenTextDocument, _>(tdr.clone()),
          registration::<lsp_types::notification::DidCloseTextDocument, _>(tdr.clone()),
          registration::<lsp_types::notification::DidSaveTextDocument, _>(
            lsp_types::TextDocumentSaveRegistrationOptions {
              include_text: Some(true),
              text_document_registration_options: tdr,
            },
          ),
        ]
      }
    };
    ret.send_request::<lsp_types::request::RegisterCapability>(lsp_types::RegistrationParams {
      registrations,
    });
    ret
  }

  fn send_request<R>(&mut self, params: R::Params)
  where
    R: lsp_types::request::Request,
  {
    let req = self
      .req_queue
      .outgoing
      .register(R::METHOD.to_owned(), params, ());
    self.send(req.into())
  }

  fn send_response(&mut self, res: Response) {
    match self.req_queue.incoming.complete(res.id.clone()) {
      Some(()) => self.send(res.into()),
      None => log::warn!("tried to respond to a non-queued request: {res:?}"),
    }
  }

  fn send_notification<N>(&self, params: N::Params)
  where
    N: lsp_types::notification::Notification,
  {
    let notif = Notification::new(N::METHOD.to_owned(), params);
    self.send(notif.into())
  }

  pub(crate) fn handle_request(&mut self, req: Request) {
    log::info!("got request: {req:?}");
    self.req_queue.incoming.register(req.id.clone(), ());
    let mut root = match self.root.take() {
      Some(x) => x,
      None => {
        // TODO could improve support for no-root mode.
        log::warn!("can't handle request with no root");
        self.send_response(Response::new_ok(req.id, None::<()>));
        return;
      }
    };
    match self.handle_request_(&mut root, req) {
      ControlFlow::Break(Ok(())) => {}
      ControlFlow::Break(Err(e)) => log::error!("couldn't handle request: {e}"),
      ControlFlow::Continue(req) => log::warn!("unhandled request: {req:?}"),
    }
    self.root = Some(root);
  }

  fn handle_request_(
    &mut self,
    root: &mut Root,
    mut r: Request,
  ) -> ControlFlow<Result<()>, Request> {
    r = try_request::<lsp_types::request::HoverRequest, _>(r, |id, params| {
      let params = params.text_document_position_params;
      let pos = text_doc_pos_params(&self.file_system, root, params)?;
      let res = self
        .analysis
        .get_md(pos, self.options.show_token_hover)
        .map(|(value, range)| lsp_types::Hover {
          contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value,
          }),
          range: Some(lsp_range(range)),
        });
      self.send_response(Response::new_ok(id, res));
      Ok(())
    })?;
    r = try_request::<lsp_types::request::GotoDefinition, _>(r, |id, params| {
      let params = params.text_document_position_params;
      let pos = text_doc_pos_params(&self.file_system, root, params)?;
      let res = self
        .analysis
        .get_def(pos)
        .and_then(|range| lsp_location(root, range).map(lsp_types::GotoDefinitionResponse::Scalar));
      self.send_response(Response::new_ok(id, res));
      Ok(())
    })?;
    r = try_request::<lsp_types::request::GotoTypeDefinition, _>(r, |id, params| {
      let params = params.text_document_position_params;
      let pos = text_doc_pos_params(&self.file_system, root, params)?;
      let locs: Vec<_> = self
        .analysis
        .get_ty_defs(pos)
        .into_iter()
        .flatten()
        .filter_map(|range| lsp_location(root, range))
        .collect();
      let res = (!locs.is_empty()).then_some(lsp_types::GotoDefinitionResponse::Array(locs));
      self.send_response(Response::new_ok(id, res));
      Ok(())
    })?;
    r = try_request::<lsp_types::request::CodeActionRequest, _>(r, |id, params| {
      let url = params.text_document.uri;
      let path = url_to_path_id(&self.file_system, root, &url)?;
      let range = analysis_range(params.range);
      let mut actions = Vec::<lsp_types::CodeActionOrCommand>::new();
      if let Some((range, new_text)) = self.analysis.fill_case(path.wrap(range.start)) {
        actions.push(lsp_types::CodeActionOrCommand::CodeAction(
          lsp_types::CodeAction {
            title: "Fill case".to_owned(),
            kind: Some(lsp_types::CodeActionKind::QUICKFIX),
            edit: Some(lsp_types::WorkspaceEdit {
              document_changes: Some(lsp_types::DocumentChanges::Edits(vec![
                lsp_types::TextDocumentEdit {
                  text_document: lsp_types::OptionalVersionedTextDocumentIdentifier {
                    uri: url,
                    version: None,
                  },
                  edits: vec![lsp_types::OneOf::Left(lsp_types::TextEdit {
                    range: lsp_range(range),
                    new_text,
                  })],
                },
              ])),
              ..Default::default()
            }),
            ..Default::default()
          },
        ));
      }
      self.send_response(Response::new_ok(id, actions));
      Ok(())
    })?;
    // TODO do CodeActionResolveRequest and lazily compute the edit
    ControlFlow::Continue(r)
  }

  pub(crate) fn handle_response(&mut self, res: Response) {
    log::info!("got response: {res:?}");
    match self.req_queue.outgoing.complete(res.id.clone()) {
      Some(()) => {}
      None => log::warn!("received response for non-queued request: {res:?}"),
    }
  }

  pub(crate) fn handle_notification(&mut self, notif: Notification) {
    log::info!("got notification: {notif:?}");
    match self.handle_notification_(notif) {
      ControlFlow::Break(Ok(())) => {}
      ControlFlow::Break(Err(e)) => log::error!("couldn't handle notification: {e}"),
      ControlFlow::Continue(notif) => log::warn!("unhandled notification: {notif:?}"),
    }
  }

  fn handle_notification_(&mut self, mut n: Notification) -> ControlFlow<Result<()>, Notification> {
    n = try_notification::<lsp_types::notification::DidChangeWatchedFiles, _>(n, |_| {
      match self.root.take() {
        Some(root) => {
          self.publish_diagnostics(root);
        }
        None => log::error!("no root"),
      }
    })?;
    n = try_notification::<lsp_types::notification::DidOpenTextDocument, _>(n, |params| {
      if self.root.is_none() {
        let url = params.text_document.uri;
        let text = params.text_document.text;
        self.publish_diagnostics_one(url, &text);
      }
    })?;
    n = try_notification::<lsp_types::notification::DidSaveTextDocument, _>(n, |params| {
      if self.root.is_none() {
        let url = params.text_document.uri;
        match params.text {
          Some(text) => self.publish_diagnostics_one(url, &text),
          None => log::error!("no text in did save"),
        }
      }
    })?;
    n = try_notification::<lsp_types::notification::DidCloseTextDocument, _>(n, |params| {
      if self.root.is_none() {
        let url = params.text_document.uri;
        self.send_diagnostics(url, Vec::new());
      }
    })?;
    ControlFlow::Continue(n)
  }

  fn show_error(&mut self, message: String) {
    self.send_notification::<lsp_types::notification::ShowMessage>(lsp_types::ShowMessageParams {
      typ: lsp_types::MessageType::ERROR,
      message,
    });
  }

  fn send(&self, msg: Message) {
    log::info!("sending {msg:?}");
    self.sender.send(msg).unwrap()
  }

  // diagnostics //

  fn publish_diagnostics(&mut self, mut root: Root) -> bool {
    let mut has_diagnostics = FxHashSet::<Url>::default();
    let input = elapsed::log("input::get", || {
      analysis::input::get(&self.file_system, &mut root.input)
    });
    let input = match input {
      Ok(x) => x,
      Err(e) => {
        for url in root.has_diagnostics.drain() {
          self.send_diagnostics(url, Vec::new());
        }
        let did_send_as_diagnostic = if e.path().is_file() {
          match file_url(e.path()) {
            Ok(url) => {
              root.has_diagnostics.insert(url.clone());
              self.send_diagnostics(url, vec![diagnostic(e.to_string(), e.range(), e.to_code())]);
              true
            }
            Err(_) => false,
          }
        } else {
          false
        };
        if !did_send_as_diagnostic {
          self.show_error(format!("{}: {}", e.path().display(), e));
        }
        self.root = Some(root);
        return false;
      }
    };
    let got_many = elapsed::log("get_many", || self.analysis.get_many(&input));
    for (path_id, errors) in got_many {
      let path = root.input.as_paths().get_path(path_id);
      let url = match file_url(path.as_path()) {
        Ok(x) => x,
        Err(e) => {
          log::error!("{e:#}");
          continue;
        }
      };
      let ds = diagnostics(errors);
      if ds.is_empty() || has_diagnostics.len() >= MAX_FILES_WITH_ERRORS {
        continue;
      }
      has_diagnostics.insert(url.clone());
      self.send_diagnostics(url, ds);
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

  fn publish_diagnostics_one(&mut self, url: Url, text: &str) {
    self.send_diagnostics(url, diagnostics(self.analysis.get_one(text)));
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
}

fn try_request<R, F>(req: Request, f: F) -> ControlFlow<Result<()>, Request>
where
  R: lsp_types::request::Request,
  F: FnOnce(RequestId, R::Params) -> Result<()>,
{
  match req.extract::<R::Params>(R::METHOD) {
    Ok((id, params)) => ControlFlow::Break(f(id, params)),
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

fn canonical_path_buf<F>(fs: &F, url: &Url) -> Result<paths::CanonicalPathBuf>
where
  F: paths::FileSystem,
{
  if url.scheme() != "file" {
    bail!("not a file url")
  }
  match url.to_file_path() {
    Ok(pb) => Ok(fs.canonicalize(pb.as_path())?),
    Err(()) => bail!("invalid url"),
  }
}

fn file_url(path: &std::path::Path) -> Result<Url> {
  Url::parse(&format!("file://{}", path.display())).with_context(|| "couldn't parse URL")
}

fn diagnostics(errors: Vec<analysis::Error>) -> Vec<lsp_types::Diagnostic> {
  errors
    .into_iter()
    .map(|err| diagnostic(err.message, Some(err.range), err.code))
    .collect()
}

fn diagnostic(message: String, range: Option<analysis::Range>, code: u16) -> lsp_types::Diagnostic {
  let href =
    Url::parse(&format!("{}#{}", analysis::ERRORS_URL, code)).expect("couldn't parse error URL");
  lsp_types::Diagnostic {
    range: range.map(lsp_range).unwrap_or_default(),
    severity: Some(lsp_types::DiagnosticSeverity::ERROR),
    code: Some(lsp_types::NumberOrString::Number(code.into())),
    code_description: Some(lsp_types::CodeDescription { href }),
    source: Some("Millet".to_owned()),
    message,
    related_information: None,
    tags: None,
    data: None,
  }
}

fn lsp_range(range: analysis::Range) -> lsp_types::Range {
  lsp_types::Range {
    start: lsp_position(range.start),
    end: lsp_position(range.end),
  }
}

fn lsp_position(pos: analysis::Position) -> lsp_types::Position {
  lsp_types::Position {
    line: pos.line,
    character: pos.character,
  }
}

fn lsp_location(
  root: &Root,
  range: paths::WithPath<analysis::Range>,
) -> Option<lsp_types::Location> {
  let uri = match file_url(root.input.as_paths().get_path(range.path).as_path()) {
    Ok(x) => x,
    Err(e) => {
      log::error!("{e:#}");
      return None;
    }
  };
  Some(lsp_types::Location {
    uri,
    range: lsp_range(range.val),
  })
}

fn analysis_position(pos: lsp_types::Position) -> analysis::Position {
  analysis::Position {
    line: pos.line,
    character: pos.character,
  }
}

fn analysis_range(range: lsp_types::Range) -> analysis::Range {
  analysis::Range {
    start: analysis_position(range.start),
    end: analysis_position(range.end),
  }
}

fn url_to_path_id<F>(fs: &F, root: &mut Root, url: &Url) -> Result<paths::PathId>
where
  F: paths::FileSystem,
{
  root
    .input
    .as_mut_paths()
    .get_id(&canonical_path_buf(fs, url)?)
    .with_context(|| "not in root")
}

fn text_doc_pos_params<F>(
  fs: &F,
  root: &mut Root,
  params: lsp_types::TextDocumentPositionParams,
) -> Result<paths::WithPath<analysis::Position>>
where
  F: paths::FileSystem,
{
  let path = url_to_path_id(fs, root, &params.text_document.uri)?;
  let pos = analysis_position(params.position);
  Ok(path.wrap(pos))
}

fn registration<N, T>(options: T) -> lsp_types::Registration
where
  N: lsp_types::notification::Notification,
  T: serde::Serialize,
{
  lsp_types::Registration {
    id: N::METHOD.to_owned(),
    method: N::METHOD.to_owned(),
    register_options: Some(serde_json::to_value(options).unwrap()),
  }
}
