//! See [`State`].
//!
//! TODO progress indicator?

use anyhow::{anyhow, bail, Context, Result};
use crossbeam_channel::Sender;
use diagnostic_util::Severity;
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
const LEARN_MORE: &str = "Learn more";

struct Root {
  paths_root: paths::Root,
  has_diagnostics: FxHashSet<Url>,
  registered_for_watched_files: bool,
}

/// The state of the language server. Only this may do IO. (Well, also the [`lsp_server`] channels
/// that communicate over stdin and stdout.)
pub(crate) struct State {
  root: Option<Root>,
  sender: Sender<Message>,
  req_queue: ReqQueue<(), Option<u16>>,
  analysis: analysis::Analysis,
  file_system: paths::RealFileSystem,
  options: config::Options,
}

impl State {
  pub(crate) fn new(init: lsp_types::InitializeParams, sender: Sender<Message>) -> Self {
    let file_system = paths::RealFileSystem::default();
    let options: config::Options = init
      .initialization_options
      .and_then(|v| match serde_json::from_value(v) {
        Ok(x) => Some(x),
        Err(e) => {
          log::warn!("invalid initialization_options: {e}");
          None
        }
      })
      .unwrap_or_default();
    let mut root = init
      .root_uri
      .map(|url| canonical_path_buf(&file_system, &url).map_err(|e| (e, url)))
      .transpose();
    let mut ret = Self {
      // do this convoluted incantation because we need `ret` to show the error in the `Err` case.
      root: root.as_mut().ok().and_then(Option::take).map(|root_path| Root {
        paths_root: paths::Root::new(root_path),
        has_diagnostics: FxHashSet::default(),
        registered_for_watched_files: false,
      }),
      sender,
      req_queue: ReqQueue::default(),
      analysis: analysis::Analysis::new(mlb_statics::StdBasis::full(), config::ErrorLines::Many),
      file_system,
      options,
    };
    if let Err((e, url)) = root {
      ret.show_error(format!("cannot initialize workspace root {url}: {e:#}"), 1996);
    }
    let dynamic_registration = init
      .capabilities
      .workspace
      .and_then(|x| x.file_operations?.dynamic_registration)
      .unwrap_or_default();
    if dynamic_registration {
      let mut registrations = match ret.root.take() {
        Some(mut root) => {
          let watchers = vec![lsp_types::FileSystemWatcher {
            // not sure if possible to only listen to millet.toml. "nested alternate groups are not
            // allowed" at time of writing
            glob_pattern: format!(
              "{}/**/*.{{sml,sig,fun,cm,mlb,toml}}",
              root.paths_root.as_path().display()
            ),
            kind: None,
          }];
          root.registered_for_watched_files = true;
          ret.publish_diagnostics(root, None);
          vec![registration::<lsp_types::notification::DidChangeWatchedFiles, _>(
            lsp_types::DidChangeWatchedFilesRegistrationOptions { watchers },
          )]
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
      if ret.options.diagnostics_on_change {
        registrations.push(registration::<lsp_types::notification::DidChangeTextDocument, _>(
          lsp_types::TextDocumentChangeRegistrationOptions {
            document_selector: None,
            // TODO lsp_types::TextDocumentSyncKind::FULL.into() doesn't work, and this field is
            // i32 for some reason.
            sync_kind: 1,
          },
        ));
      }
      ret.send_request::<lsp_types::request::RegisterCapability>(
        lsp_types::RegistrationParams { registrations },
        None,
      );
    }
    if ret.root.as_ref().map_or(true, |r| !r.registered_for_watched_files) {
      log::warn!("millet will not necessarily receive notifications when files change on-disk.");
      log::warn!("this means the internal state of millet can get out of sync with what is");
      log::warn!("actually on disk, e.g. when using `git checkout` or other means of modifying");
      log::warn!("files not via the language client (i.e. the editor millet is attached to).");
    }
    ret
  }

  fn send_request<R>(&mut self, params: R::Params, data: Option<u16>)
  where
    R: lsp_types::request::Request,
  {
    let req = self.req_queue.outgoing.register(R::METHOD.to_owned(), params, data);
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
      let res = self.analysis.get_md(pos, self.options.show_token_hover).map(|(value, range)| {
        lsp_types::Hover {
          contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value,
          }),
          range: Some(lsp_range(range)),
        }
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
        actions.push(lsp_types::CodeActionOrCommand::CodeAction(lsp_types::CodeAction {
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
        }));
      }
      self.send_response(Response::new_ok(id, actions));
      Ok(())
    })?;
    // TODO do CodeActionResolveRequest and lazily compute the edit
    ControlFlow::Continue(r)
  }

  pub(crate) fn handle_response(&mut self, res: Response) {
    log::info!("got response: {res:?}");
    let data = match self.req_queue.outgoing.complete(res.id.clone()) {
      Some(x) => x,
      None => {
        log::warn!("received response for non-queued request: {res:?}");
        return;
      }
    };
    let code = match data {
      Some(x) => x,
      None => {
        log::info!("no error code associated with this request");
        return;
      }
    };
    let val = match res.result {
      Some(x) => x,
      None => {
        log::info!("user did not click to look at the error URL");
        return;
      }
    };
    let item = match serde_json::from_value::<lsp_types::MessageActionItem>(val) {
      Ok(x) => x,
      Err(e) => {
        log::error!("registered an error code, but got no message action item: {e}");
        return;
      }
    };
    if item.title != LEARN_MORE {
      log::warn!("unknown item.title: {}", item.title);
      return;
    }
    self.send_request::<lsp_types::request::ShowDocument>(
      lsp_types::ShowDocumentParams {
        uri: error_url(code),
        external: Some(true),
        take_focus: Some(true),
        selection: None,
      },
      None,
    );
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
          self.publish_diagnostics(root, None);
          Ok(())
        }
        None => bail!("can't handle DidChangeWatchedFiles with no root"),
      }
    })?;
    n = try_notification::<lsp_types::notification::DidChangeTextDocument, _>(n, |params| {
      let url = params.text_document.uri;
      let mut changes = params.content_changes;
      let change = match changes.pop() {
        Some(x) => x,
        None => bail!("no content changes"),
      };
      if !changes.is_empty() {
        bail!("not exactly 1 content change");
      }
      if change.range.is_some() {
        bail!("not a full document change");
      }
      let contents = change.text;
      match self.root.take() {
        Some(mut root) => {
          let path = url_to_path_id(&self.file_system, &mut root, &url)?;
          self.publish_diagnostics(root, Some((path, contents)));
        }
        None => {
          self.publish_diagnostics_one(url, &contents);
        }
      }
      Ok(())
    })?;
    n = try_notification::<lsp_types::notification::DidOpenTextDocument, _>(n, |params| {
      if self.root.is_none() {
        let url = params.text_document.uri;
        let text = params.text_document.text;
        self.publish_diagnostics_one(url, &text);
      }
      Ok(())
    })?;
    n = try_notification::<lsp_types::notification::DidSaveTextDocument, _>(n, |params| {
      match self.root.take() {
        None => match params.text {
          Some(text) => {
            let url = params.text_document.uri;
            self.publish_diagnostics_one(url, &text);
          }
          None => bail!("no text for DidSaveTextDocument"),
        },
        Some(root) => {
          if root.registered_for_watched_files {
            log::warn!("ignoring DidSaveTextDocument since we registered for watched file events");
            self.root = Some(root);
          } else {
            self.publish_diagnostics(root, None);
          }
        }
      }
      Ok(())
    })?;
    n = try_notification::<lsp_types::notification::DidCloseTextDocument, _>(n, |params| {
      if self.root.is_none() {
        let url = params.text_document.uri;
        self.send_diagnostics(url, Vec::new());
      }
      Ok(())
    })?;
    ControlFlow::Continue(n)
  }

  fn send(&self, msg: Message) {
    log::info!("sending {msg:?}");
    self.sender.send(msg).unwrap()
  }

  // diagnostics //

  /// also gets input from the filesystem and puts the root back onto self
  fn publish_diagnostics(
    &mut self,
    mut root: Root,
    extra: Option<(paths::PathId, String)>,
  ) -> bool {
    let mut has_diagnostics = FxHashSet::<Url>::default();
    let input = elapsed::log("Input::new", || {
      analysis::input::Input::new(&self.file_system, &mut root.paths_root)
    });
    let mut input = match input {
      Ok(x) => x,
      Err(e) => {
        for url in root.has_diagnostics.drain() {
          self.send_diagnostics(url, Vec::new());
        }
        let did_send_as_diagnostic = if e.path().is_file() {
          match file_url(e.path()) {
            Ok(url) => {
              root.has_diagnostics.insert(url.clone());
              self.send_diagnostics(
                url,
                vec![diagnostic(e.to_string(), e.range(), e.code(), e.severity())],
              );
              true
            }
            Err(_) => false,
          }
        } else {
          false
        };
        if !did_send_as_diagnostic {
          self.show_error(format!("{}: {}", e.path().display(), e), e.code());
        }
        self.root = Some(root);
        return false;
      }
    };
    if let Some((path, contents)) = extra {
      input.override_source(path, contents);
    }
    let got_many = elapsed::log("get_many", || self.analysis.get_many(&input));
    for (path_id, errors) in got_many {
      let path = root.paths_root.get_path(path_id);
      let url = match file_url(path.as_path()) {
        Ok(x) => x,
        Err(e) => {
          log::error!("couldn't get path as a file url: {e:#}");
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
      lsp_types::PublishDiagnosticsParams { uri: url, diagnostics, version: None },
    );
  }

  fn show_error(&mut self, message: String, code: u16) {
    self.send_request::<lsp_types::request::ShowMessageRequest>(
      lsp_types::ShowMessageRequestParams {
        typ: lsp_types::MessageType::ERROR,
        message,
        actions: Some(vec![lsp_types::MessageActionItem {
          title: LEARN_MORE.to_owned(),
          properties: Default::default(),
        }]),
      },
      Some(code),
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
  F: FnOnce(N::Params) -> Result<()>,
{
  match notif.extract::<N::Params>(N::METHOD) {
    Ok(params) => ControlFlow::Break(f(params)),
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
    bail!("not a file url: {url}")
  }
  match url.to_file_path() {
    Ok(pb) => Ok(fs.canonicalize(pb.as_path())?),
    Err(()) => bail!("couldn't make a URL into a file path: {url}"),
  }
}

fn file_url(path: &std::path::Path) -> Result<Url> {
  Url::parse(&format!("file://{}", path.display()))
    .with_context(|| format!("couldn't parse path into a URL: {path:?}"))
}

fn diagnostics(errors: Vec<diagnostic_util::Error>) -> Vec<lsp_types::Diagnostic> {
  errors
    .into_iter()
    .map(|err| diagnostic(err.message, Some(err.range), err.code, err.severity))
    .collect()
}

fn error_url(code: u16) -> Url {
  Url::parse(&format!("{}#{}", diagnostic_util::ERRORS_URL, code))
    .expect("couldn't parse error URL")
}

fn diagnostic(
  message: String,
  range: Option<text_pos::Range>,
  code: u16,
  severity: Severity,
) -> lsp_types::Diagnostic {
  lsp_types::Diagnostic {
    range: range.map(lsp_range).unwrap_or_default(),
    severity: Some(match severity {
      Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
      Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
    }),
    code: Some(lsp_types::NumberOrString::Number(code.into())),
    code_description: Some(lsp_types::CodeDescription { href: error_url(code) }),
    source: Some("Millet".to_owned()),
    message,
    related_information: None,
    tags: None,
    data: None,
  }
}

fn lsp_range(range: text_pos::Range) -> lsp_types::Range {
  lsp_types::Range { start: lsp_position(range.start), end: lsp_position(range.end) }
}

fn lsp_position(pos: text_pos::Position) -> lsp_types::Position {
  lsp_types::Position { line: pos.line, character: pos.character }
}

fn lsp_location(
  root: &Root,
  range: paths::WithPath<text_pos::Range>,
) -> Option<lsp_types::Location> {
  let uri = match file_url(root.paths_root.get_path(range.path).as_path()) {
    Ok(x) => x,
    Err(e) => {
      log::error!("couldn't get path as a file url: {e:#}");
      return None;
    }
  };
  Some(lsp_types::Location { uri, range: lsp_range(range.val) })
}

fn analysis_position(pos: lsp_types::Position) -> text_pos::Position {
  text_pos::Position { line: pos.line, character: pos.character }
}

fn analysis_range(range: lsp_types::Range) -> text_pos::Range {
  text_pos::Range { start: analysis_position(range.start), end: analysis_position(range.end) }
}

fn url_to_path_id<F>(fs: &F, root: &mut Root, url: &Url) -> Result<paths::PathId>
where
  F: paths::FileSystem,
{
  root.paths_root.get_id(&canonical_path_buf(fs, url)?).with_context(|| "not in root")
}

fn text_doc_pos_params<F>(
  fs: &F,
  root: &mut Root,
  params: lsp_types::TextDocumentPositionParams,
) -> Result<paths::WithPath<text_pos::Position>>
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
