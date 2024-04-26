//! Initialize a new server.

use crate::state::{Mode, Root, St};
use crate::{convert, cx::Cx, diagnostics};
use crossbeam_channel::Sender;
use diagnostic::Code;
use fast_hash::FxHashSet;
use lsp_server::{Message, ReqQueue};

pub(crate) fn init(init: lsp_types::InitializeParams, sender: Sender<Message>) -> St {
  let options: config::init::Options = init
    .initialization_options
    .and_then(|v| match serde_json::from_value(v) {
      Ok(x) => Some(x),
      Err(e) => {
        log::warn!("invalid initialization_options: {e}");
        None
      }
    })
    .unwrap_or_default();
  let analysis_options = analysis::Options {
    lines: config::DiagnosticLines::Many,
    ignore: options.diagnostics.ignore,
    format: options.format,
  };
  let analysis = analysis::Analysis::new(analysis::StdBasis::full(), analysis_options);
  let mut cx = Cx {
    options,
    registered_for_watched_files: false,
    paths: paths::Store::new(),
    open_paths: paths::PathSet::default(),
    fs: paths::RealFileSystem::default(),
    sender,
    req_queue: ReqQueue::default(),
  };
  let last_workspace_folder = init
    .workspace_folders
    .and_then(|mut xs| {
      let ret = xs.pop();
      if !xs.is_empty() {
        log::warn!("we only support the last workspace folder");
      }
      ret
    })
    .map(|x| x.uri.clone());
  #[allow(deprecated)]
  let root_uri = last_workspace_folder.or(init.root_uri);
  let mut root =
    root_uri.map(|url| convert::clean_path_buf(&url).map_err(|e| (e, url))).transpose();
  let mut ret = St {
    // do this convoluted incantation because we need `ret` to show the error in the `Err` case.
    mode: match root.as_mut().ok().and_then(Option::take) {
      Some(path) => {
        let input = cx.get_input(path.as_clean_path());
        Mode::Root(Box::new(Root { path, input }))
      }
      None => Mode::NoRoot,
    },
    cx,
    analysis,
    has_diagnostics: FxHashSet::default(),
  };
  if let Err((e, url)) = root {
    ret.cx.show_error(format!("cannot initialize workspace root {url}: {e:#}"), Code::n(1018));
  }
  let want_file_ops = ret.cx.options.fs_watcher.0
    && init
      .capabilities
      .workspace
      .and_then(|x| x.file_operations?.dynamic_registration)
      .unwrap_or_default();
  if want_file_ops {
    if let Mode::Root(root) = &ret.mode {
      // we'd like to activate on millet.toml, but "nested alternate groups are not allowed" at time
      // of writing, and we'd rather not activate on all toml.
      let watchers = vec![lsp_types::FileSystemWatcher {
        glob_pattern: lsp_types::GlobPattern::Relative(lsp_types::RelativePattern {
          base_uri: lsp_types::OneOf::Right(convert::file_url(root.path.as_path()).unwrap()),
          pattern: "**/*.{sml,sig,fun,cm,mlb}".to_owned(),
        }),
        kind: None,
      }];
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
