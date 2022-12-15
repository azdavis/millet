//! See [`Cx`].

use crate::helpers;
use crossbeam_channel::Sender;
use diagnostic_util::Code;
use fast_hash::FxHashSet;
use lsp_server::{Message, Notification, ReqQueue, Response};
use lsp_types::Url;

pub(crate) const LEARN_MORE: &str = "Learn more";

/// The context, kind of like "semi-permanent" state.
///
/// Some things on this are totally immutable after initialization. Other things are mutable, but
/// nothing on this will ever get "replaced" entirely; instead, _if_ it's mutable, _when_ it's
/// mutate, it'll only be "tweaked" a bit.
pub(crate) struct Cx {
  pub(crate) options: config::Options,
  pub(crate) registered_for_watched_files: bool,
  pub(crate) store: paths::Store,
  pub(crate) file_system: paths::RealFileSystem,
  pub(crate) sender: Sender<Message>,
  pub(crate) req_queue: ReqQueue<(), Option<Code>>,
}

impl Cx {
  pub(crate) fn send(&self, msg: Message) {
    log::info!("sending {msg:?}");
    self.sender.send(msg).unwrap();
  }

  pub(crate) fn send_request<R>(&mut self, params: R::Params, data: Option<Code>)
  where
    R: lsp_types::request::Request,
  {
    let req = self.req_queue.outgoing.register(R::METHOD.to_owned(), params, data);
    self.send(req.into());
  }

  pub(crate) fn send_response(&mut self, res: Response) {
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
    self.send(notif.into());
  }

  pub(crate) fn send_diagnostics(&mut self, url: Url, diagnostics: Vec<lsp_types::Diagnostic>) {
    self.send_notification::<lsp_types::notification::PublishDiagnostics>(
      lsp_types::PublishDiagnosticsParams { uri: url, diagnostics, version: None },
    );
  }

  pub(crate) fn show_error(&mut self, message: String, code: Code) {
    self.send_request::<lsp_types::request::ShowMessageRequest>(
      lsp_types::ShowMessageRequestParams {
        typ: lsp_types::MessageType::ERROR,
        message,
        actions: Some(vec![lsp_types::MessageActionItem {
          title: LEARN_MORE.to_owned(),
          properties: std::collections::HashMap::new(),
        }]),
      },
      Some(code),
    );
  }

  pub(crate) fn try_get_input(
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
