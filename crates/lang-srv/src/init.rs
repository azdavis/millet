//! Initialize a new server.

use crate::state::{Mode, Root, St};
use crate::{convert, cx::Cx, diagnostics};
use crossbeam_channel::Sender;
use diagnostic_util::Code;
use fast_hash::{FxHashMap, FxHashSet};
use lsp_server::{Message, ReqQueue};
use lsp_types::Url;

pub(crate) fn init(init: lsp_types::InitializeParams, sender: Sender<Message>) -> St {
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
  let mut sp = Cx {
    options,
    registered_for_watched_files: false,
    store: paths::Store::new(),
    file_system: paths::RealFileSystem::default(),
    sender,
    req_queue: ReqQueue::default(),
  };
  let mut root = init
    .root_uri
    .map(|url| convert::canonical_path_buf(&sp.file_system, &url).map_err(|e| (e, url)))
    .transpose();
  let mut has_diagnostics = FxHashSet::<Url>::default();
  let mut ret = St {
    // do this convoluted incantation because we need `ret` to show the error in the `Err` case.
    mode: match root.as_mut().ok().and_then(Option::take) {
      Some(path) => {
        let input = sp.try_get_input(&path, &mut has_diagnostics);
        Mode::Root(Root { path, input })
      }
      None => Mode::NoRoot(FxHashMap::default()),
    },
    cx: sp,
    analysis,
    has_diagnostics,
  };
  if let Err((e, url)) = root {
    ret.cx.show_error(format!("cannot initialize workspace root {url}: {e:#}"), Code::n(1018));
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
        convert::registration::<lsp_types::notification::DidChangeWatchedFiles, _>(
          lsp_types::DidChangeWatchedFilesRegistrationOptions { watchers },
        );
      ret.cx.send_request::<lsp_types::request::RegisterCapability>(
        lsp_types::RegistrationParams { registrations: vec![did_changed_registration] },
        None,
      );
      ret.cx.registered_for_watched_files = true;
    };
  }
  diagnostics::try_publish(&mut ret);
  if !ret.cx.registered_for_watched_files {
    log::warn!("millet will not necessarily receive notifications when files change on-disk.");
    log::warn!("this means the internal state of millet can get out of sync with what is");
    log::warn!("actually on disk, e.g. when using `git checkout` or other means of modifying");
    log::warn!("files not via the language client (i.e. the editor millet is attached to).");
  }
  ret
}