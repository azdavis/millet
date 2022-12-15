//! The main mutable state of the language server.

use crate::helpers;
use anyhow::{bail, Result};
use crossbeam_channel::Sender;
use diagnostic_util::Code;
use fast_hash::{FxHashMap, FxHashSet};
use lsp_server::{Message, Notification, ReqQueue, Request, Response};
use lsp_types::Url;
use std::ops::ControlFlow;

const LEARN_MORE: &str = "Learn more";

/// The state.
pub(crate) struct State {
  mode: Mode,
  sp: SPState,
  analysis: analysis::Analysis,
  has_diagnostics: FxHashSet<Url>,
}

impl State {
  pub(crate) fn new(init: lsp_types::InitializeParams, sender: Sender<Message>) -> Self {
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
    let analysis = analysis::Analysis::new(
      analysis::StdBasis::Full,
      config::ErrorLines::Many,
      options.diagnostics_filter,
      options.format,
    );
    let mut sp = SPState {
      options,
      registered_for_watched_files: false,
      store: paths::Store::new(),
      file_system: paths::RealFileSystem::default(),
      sender,
      req_queue: ReqQueue::default(),
    };
    let mut root = init
      .root_uri
      .map(|url| helpers::canonical_path_buf(&sp.file_system, &url).map_err(|e| (e, url)))
      .transpose();
    let mut has_diagnostics = FxHashSet::<Url>::default();
    let mut ret = Self {
      // do this convoluted incantation because we need `ret` to show the error in the `Err` case.
      mode: match root.as_mut().ok().and_then(Option::take) {
        Some(path) => {
          let input = sp.try_get_input(&path, &mut has_diagnostics);
          Mode::Root(Root { path, input })
        }
        None => Mode::NoRoot(FxHashMap::default()),
      },
      sp,
      analysis,
      has_diagnostics,
    };
    if let Err((e, url)) = root {
      ret.sp.show_error(format!("cannot initialize workspace root {url}: {e:#}"), Code::n(1018));
    }
    let dynamic_registration = init
      .capabilities
      .workspace
      .and_then(|x| x.file_operations?.dynamic_registration)
      .unwrap_or_default();
    if dynamic_registration {
      if let Mode::Root(root) = &ret.mode {
        // we'd like to only listen to millet.toml, not all toml, but "nested alternate groups are
        // not allowed" at time of writing.
        let glob_pattern =
          format!("{}/**/*.{{sml,sig,fun,cm,mlb,toml}}", root.path.as_path().display());
        let watchers = vec![lsp_types::FileSystemWatcher { glob_pattern, kind: None }];
        let did_changed_registration =
          helpers::registration::<lsp_types::notification::DidChangeWatchedFiles, _>(
            lsp_types::DidChangeWatchedFilesRegistrationOptions { watchers },
          );
        ret.sp.send_request::<lsp_types::request::RegisterCapability>(
          lsp_types::RegistrationParams { registrations: vec![did_changed_registration] },
          None,
        );
        ret.sp.registered_for_watched_files = true;
      };
    }
    ret.try_publish_diagnostics();
    if !ret.sp.registered_for_watched_files {
      log::warn!("millet will not necessarily receive notifications when files change on-disk.");
      log::warn!("this means the internal state of millet can get out of sync with what is");
      log::warn!("actually on disk, e.g. when using `git checkout` or other means of modifying");
      log::warn!("files not via the language client (i.e. the editor millet is attached to).");
    }
    ret
  }

  pub(crate) fn handle_request(&mut self, req: Request) {
    log::info!("got request: {req:?}");
    self.sp.req_queue.incoming.register(req.id.clone(), ());
    match self.handle_request_(req) {
      ControlFlow::Break(Ok(())) => {}
      ControlFlow::Break(Err(e)) => log::error!("couldn't handle request: {e}"),
      ControlFlow::Continue(req) => log::warn!("unhandled request: {req:?}"),
    }
  }

  fn handle_request_(&mut self, mut r: Request) -> ControlFlow<Result<()>, Request> {
    r = helpers::try_req::<lsp_types::request::HoverRequest, _>(r, |id, params| {
      let params = params.text_document_position_params;
      let pos = helpers::text_doc_pos_params(&self.sp.file_system, &mut self.sp.store, params)?;
      let res =
        self.analysis.get_md(pos, self.sp.options.show_token_hover).map(|(value, range)| {
          lsp_types::Hover {
            contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
              kind: lsp_types::MarkupKind::Markdown,
              value,
            }),
            range: Some(helpers::lsp_range(range)),
          }
        });
      self.sp.send_response(Response::new_ok(id, res));
      Ok(())
    })?;
    r = helpers::try_req::<lsp_types::request::GotoDefinition, _>(r, |id, params| {
      let params = params.text_document_position_params;
      let pos = helpers::text_doc_pos_params(&self.sp.file_system, &mut self.sp.store, params)?;
      let res = self.analysis.get_def(pos).and_then(|range| {
        helpers::lsp_location(&self.sp.store, range).map(lsp_types::GotoDefinitionResponse::Scalar)
      });
      self.sp.send_response(Response::new_ok(id, res));
      Ok(())
    })?;
    r = helpers::try_req::<lsp_types::request::GotoTypeDefinition, _>(r, |id, params| {
      let params = params.text_document_position_params;
      let pos = helpers::text_doc_pos_params(&self.sp.file_system, &mut self.sp.store, params)?;
      let locs: Vec<_> = self
        .analysis
        .get_ty_defs(pos)
        .into_iter()
        .flatten()
        .filter_map(|range| helpers::lsp_location(&self.sp.store, range))
        .collect();
      let res = (!locs.is_empty()).then_some(lsp_types::GotoDefinitionResponse::Array(locs));
      self.sp.send_response(Response::new_ok(id, res));
      Ok(())
    })?;
    r = helpers::try_req::<lsp_types::request::CodeActionRequest, _>(r, |id, params| {
      let url = params.text_document.uri;
      let path = helpers::url_to_path_id(&self.sp.file_system, &mut self.sp.store, &url)?;
      let range = helpers::analysis_range(params.range);
      let mut actions = Vec::<lsp_types::CodeActionOrCommand>::new();
      if let Some((range, new_text)) = self.analysis.fill_case(path.wrap(range.start)) {
        actions.push(helpers::quick_fix("Fill case".to_owned(), url, range, new_text));
      }
      self.sp.send_response(Response::new_ok(id, actions));
      Ok(())
    })?;
    r = helpers::try_req::<lsp_types::request::Formatting, _>(r, |id, params| {
      let url = params.text_document.uri;
      let path = helpers::url_to_path_id(&self.sp.file_system, &mut self.sp.store, &url)?;
      let res = self.analysis.format(path, params.options.tab_size).ok().map(|(new_text, end)| {
        vec![lsp_types::TextEdit {
          range: lsp_types::Range {
            start: lsp_types::Position { line: 0, character: 0 },
            end: helpers::lsp_position(end),
          },
          new_text,
        }]
      });
      self.sp.send_response(Response::new_ok(id, res));
      Ok(())
    })?;
    ControlFlow::Continue(r)
  }

  pub(crate) fn handle_response(&mut self, res: Response) {
    log::info!("got response: {res:?}");
    let data = match self.sp.req_queue.outgoing.complete(res.id.clone()) {
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
    self.sp.send_request::<lsp_types::request::ShowDocument>(
      lsp_types::ShowDocumentParams {
        uri: helpers::error_url(code),
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
    n = helpers::try_notif::<lsp_types::notification::DidChangeWatchedFiles, _>(n, |_| {
      match &mut self.mode {
        Mode::Root(root) => {
          root.input = self.sp.try_get_input(&root.path, &mut self.has_diagnostics);
          self.try_publish_diagnostics();
        }
        Mode::NoRoot(_) => log::warn!("ignoring DidChangeWatchedFiles with NoRoot"),
      }
      Ok(())
    })?;
    n = helpers::try_notif::<lsp_types::notification::DidChangeTextDocument, _>(n, |params| {
      let url = params.text_document.uri;
      let path = helpers::url_to_path_id(&self.sp.file_system, &mut self.sp.store, &url)?;
      match &mut self.mode {
        Mode::Root(root) => {
          let input = match &mut root.input {
            Some(x) => x,
            None => bail!("no input for DidChangeTextDocument"),
          };
          let text = match input.get_mut_source(path) {
            Some(x) => x,
            None => bail!("no source in the input for DidChangeTextDocument"),
          };
          helpers::apply_changes(text, params.content_changes);
          if self.sp.options.diagnostics_on_change {
            self.try_publish_diagnostics();
          } else if self.sp.options.format {
            // TODO this is expensive, but currently necessary to make formatting work. can we
            // make it just do it for formatting (i.e. syntax) only (no statics)?
            self.analysis.get_many(input);
          }
        }
        Mode::NoRoot(open_files) => match open_files.get_mut(&path) {
          Some(text) => {
            helpers::apply_changes(text, params.content_changes);
            if self.sp.options.diagnostics_on_change {
              let ds = helpers::diagnostics(
                self.analysis.get_one(text),
                self.sp.options.diagnostics_more_info_hint,
              );
              self.sp.send_diagnostics(url, ds);
            }
          }
          None => bail!("no open file found for DidChangeTextDocument"),
        },
      }
      Ok(())
    })?;
    n = helpers::try_notif::<lsp_types::notification::DidOpenTextDocument, _>(n, |params| {
      if let Mode::NoRoot(open_files) = &mut self.mode {
        let url = params.text_document.uri;
        let text = params.text_document.text;
        let path = helpers::url_to_path_id(&self.sp.file_system, &mut self.sp.store, &url)?;
        let ds = helpers::diagnostics(
          self.analysis.get_one(&text),
          self.sp.options.diagnostics_more_info_hint,
        );
        self.sp.send_diagnostics(url, ds);
        open_files.insert(path, text);
      }
      Ok(())
    })?;
    n = helpers::try_notif::<lsp_types::notification::DidSaveTextDocument, _>(n, |params| {
      match &mut self.mode {
        Mode::Root(root) => {
          if self.sp.registered_for_watched_files {
            log::warn!("ignoring DidSaveTextDocument since we registered for watched file events");
          } else {
            root.input = self.sp.try_get_input(&root.path, &mut self.has_diagnostics);
            self.try_publish_diagnostics();
          }
        }
        Mode::NoRoot(open_files) => {
          if params.text.is_some() {
            log::warn!("got text for DidSaveTextDocument");
          }
          let url = params.text_document.uri;
          let path = helpers::url_to_path_id(&self.sp.file_system, &mut self.sp.store, &url)?;
          match open_files.get(&path) {
            Some(text) => {
              let ds = helpers::diagnostics(
                self.analysis.get_one(text),
                self.sp.options.diagnostics_more_info_hint,
              );
              self.sp.send_diagnostics(url, ds);
            }
            None => bail!("no open file found for DidSaveTextDocument"),
          }
        }
      }
      Ok(())
    })?;
    n = helpers::try_notif::<lsp_types::notification::DidCloseTextDocument, _>(n, |params| {
      if let Mode::NoRoot(open_files) = &mut self.mode {
        let url = params.text_document.uri;
        let path = helpers::url_to_path_id(&self.sp.file_system, &mut self.sp.store, &url)?;
        open_files.remove(&path);
        self.sp.send_diagnostics(url, Vec::new());
      }
      Ok(())
    })?;
    ControlFlow::Continue(n)
  }

  fn try_publish_diagnostics(&mut self) -> bool {
    let root = match &mut self.mode {
      Mode::Root(x) => x,
      Mode::NoRoot(_) => return false,
    };
    let input = match &mut root.input {
      Some(x) => x,
      None => return false,
    };
    let got_many = self.analysis.get_many(input);
    let mut has_diagnostics = FxHashSet::<Url>::default();
    for (path_id, errors) in got_many {
      let path = self.sp.store.get_path(path_id);
      let url = match helpers::file_url(path.as_path()) {
        Ok(x) => x,
        Err(e) => {
          log::error!("couldn't get path as a file url: {e:#}");
          continue;
        }
      };
      let ds = helpers::diagnostics(errors, self.sp.options.diagnostics_more_info_hint);
      if ds.is_empty() {
        continue;
      }
      has_diagnostics.insert(url.clone());
      self.sp.send_diagnostics(url, ds);
    }
    // iter over the old list of urls with diagnostics.
    for url in std::mem::take(&mut self.has_diagnostics) {
      if has_diagnostics.contains(&url) {
        // had old and new diagnostics. just sent the new ones.
        continue;
      }
      // had old diagnostics, but no new diagnostics. clear the old diagnostics.
      self.sp.send_diagnostics(url, Vec::new());
    }
    self.has_diagnostics = has_diagnostics;
    true
  }
}

enum Mode {
  /// We have a workspace root.
  Root(Root),
  /// We have no workspace root. We track the open files.
  NoRoot(FxHashMap<paths::PathId, String>),
}

struct Root {
  path: paths::CanonicalPathBuf,
  input: Option<input::Input>,
}

/// Semi-Permanent state. Some things on this are totally immutable after initialization. Other
/// things are mutable, but nothing on this will ever get "replaced" entirely; instead, _if_ it's
/// mutable, _when_ it's mutate, it'll only be "tweaked" a bit.
struct SPState {
  options: config::Options,
  registered_for_watched_files: bool,
  store: paths::Store,
  file_system: paths::RealFileSystem,
  sender: Sender<Message>,
  req_queue: ReqQueue<(), Option<Code>>,
}

impl SPState {
  fn send(&self, msg: Message) {
    log::info!("sending {msg:?}");
    self.sender.send(msg).unwrap()
  }

  fn send_request<R>(&mut self, params: R::Params, data: Option<Code>)
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

  fn send_diagnostics(&mut self, url: Url, diagnostics: Vec<lsp_types::Diagnostic>) {
    self.send_notification::<lsp_types::notification::PublishDiagnostics>(
      lsp_types::PublishDiagnosticsParams { uri: url, diagnostics, version: None },
    );
  }

  fn show_error(&mut self, message: String, code: Code) {
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

  fn try_get_input(
    &mut self,
    root: &paths::CanonicalPathBuf,
    has_diagnostics: &mut FxHashSet<Url>,
  ) -> Option<input::Input> {
    let input =
      elapsed::log("Input::new", || input::Input::new(&self.file_system, &mut self.store, root));
    let err = match input {
      Ok(x) => return Some(x),
      Err(x) => x,
    };
    // clear all current diagnostics. if there is an error getting input, it is the only error.
    for url in std::mem::take(has_diagnostics) {
      self.send_diagnostics(url, Vec::new());
    }
    let did_send_as_diagnostic = if err.abs_path().is_file() {
      match helpers::file_url(err.abs_path()) {
        Ok(url) => {
          has_diagnostics.insert(url.clone());
          self.send_diagnostics(
            url,
            vec![helpers::diagnostic(
              err.display(root.as_path()).to_string(),
              err.range(),
              err.code(),
              err.severity(),
              self.options.diagnostics_more_info_hint,
            )],
          );
          true
        }
        Err(_) => false,
      }
    } else {
      false
    };
    if !did_send_as_diagnostic {
      self.show_error(
        format!(
          "{}: {}",
          err.maybe_rel_path(root.as_path()).display(),
          err.display(root.as_path())
        ),
        err.code(),
      );
    }
    None
  }
}
