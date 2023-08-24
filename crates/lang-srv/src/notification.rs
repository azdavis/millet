//! Handle notifications.

use crate::cx::Cx;
use crate::state::{Mode, St};
use crate::{convert, diagnostics, helpers};
use anyhow::{bail, Result};
use lsp_server::Notification;
use paths::FileSystem as _;
use std::collections::hash_map::Entry;
use std::ops::ControlFlow;

pub(crate) fn handle(st: &mut St, notif: Notification) {
  log::info!("got notification: {notif:?}");
  match go(st, notif) {
    ControlFlow::Break(Ok(())) => {}
    ControlFlow::Break(Err(e)) => log::error!("couldn't handle notification: {e}"),
    ControlFlow::Continue(notif) => log::warn!("unhandled notification: {notif:?}"),
  }
}

/// try to incrementally update the input instead of throwing away and re-computing the whole input.
///
/// only handles some common cases.
fn try_update_input(
  cx: &mut Cx,
  input: &mut input::Input,
  changes: Vec<lsp_types::FileEvent>,
) -> Result<Vec<paths::PathId>> {
  let mut ret = Vec::<paths::PathId>::with_capacity(changes.len());
  for change in changes {
    let path = convert::canonical_path_buf(&cx.fs, &change.uri)?;
    let path_id = cx.paths.get_id(&path);
    ret.push(path_id);
    let mut entry = match input.sources.entry(path_id) {
      Entry::Occupied(x) => x,
      Entry::Vacant(_) => bail!("file {} was not a pre-existing source", path.as_path().display()),
    };
    if change.typ == lsp_types::FileChangeType::CREATED
      || change.typ == lsp_types::FileChangeType::CHANGED
    {
      let new_contents = cx.fs.read_to_string(path.as_path())?;
      entry.insert(new_contents);
    } else if change.typ == lsp_types::FileChangeType::DELETED {
      entry.remove();
    } else {
      bail!("unknown file change type {:?}", change.typ);
    }
  }
  ret.shrink_to_fit();
  Ok(ret)
}

#[allow(clippy::too_many_lines)]
fn go(st: &mut St, mut n: Notification) -> ControlFlow<Result<()>, Notification> {
  n = helpers::try_notif::<lsp_types::notification::DidChangeWatchedFiles, _>(n, |params| {
    match &mut st.mode {
      Mode::Root(root) => {
        match try_update_input(&mut st.cx, &mut root.input, params.changes) {
          Ok(_) => {
            // TODO use path ids
          }
          Err(_) => root.input = st.cx.get_input(&root.path),
        }
        diagnostics::try_publish(st);
      }
      Mode::NoRoot => bail!("unexpected DidChangeWatchedFiles with NoRoot"),
    }
    Ok(())
  })?;
  n = helpers::try_notif::<lsp_types::notification::DidChangeTextDocument, _>(n, |params| {
    let url = params.text_document.uri;
    let path = convert::url_to_path_id(&st.cx.fs, &mut st.cx.paths, &url)?;
    if let Mode::Root(root) = &mut st.mode {
      let Some(text) = root.input.sources.get_mut(&path) else {
        bail!("no source in the input for DidChangeTextDocument")
      };
      helpers::apply_changes(text, params.content_changes);
      if st.cx.options.diagnostics.on_change {
        diagnostics::try_publish(st);
      } else {
        st.analysis.update_one(&root.input, path);
      }
    }
    Ok(())
  })?;
  n = helpers::try_notif::<lsp_types::notification::DidSaveTextDocument, _>(n, |_| {
    if let Mode::Root(root) = &mut st.mode {
      if st.cx.registered_for_watched_files {
        log::warn!("ignoring DidSaveTextDocument since we registered for watched file events");
      } else {
        root.input = st.cx.get_input(&root.path);
        diagnostics::try_publish(st);
      }
    }
    Ok(())
  })?;
  ControlFlow::Continue(n)
}
