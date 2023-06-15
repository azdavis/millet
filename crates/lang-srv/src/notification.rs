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
    match st.mode.take() {
      Mode::Root(mut root) => {
        match try_update_input(&mut st.cx, &mut root.input, params.changes) {
          Ok(_) => {
            // TODO use path ids
          }
          Err(_) => root.input = st.cx.get_input(&root.path),
        }
        st.mode = Mode::Root(root);
        diagnostics::try_publish(st);
      }
      Mode::NoRoot(nr) => {
        st.mode = Mode::NoRoot(nr);
        bail!("unexpected DidChangeWatchedFiles with NoRoot");
      }
    }
    Ok(())
  })?;
  n = helpers::try_notif::<lsp_types::notification::DidChangeTextDocument, _>(n, |params| {
    let url = params.text_document.uri;
    let path = convert::url_to_path_id(&st.cx.fs, &mut st.cx.paths, &url)?;
    match &mut st.mode {
      Mode::Root(root) => {
        let text = match root.input.sources.get_mut(&path) {
          Some(x) => x,
          None => bail!("no source in the input for DidChangeTextDocument"),
        };
        helpers::apply_changes(text, params.content_changes);
        if st.cx.options.diagnostics.on_change {
          diagnostics::try_publish(st);
        }
      }
      Mode::NoRoot(open_files) => match open_files.get_mut(&path) {
        Some(text) => {
          helpers::apply_changes(text, params.content_changes);
          if st.cx.options.diagnostics.on_change {
            let ds = convert::diagnostics(
              st.analysis.get_one(text),
              st.cx.options.diagnostics.more_info_hint.0,
            );
            st.cx.send_diagnostics(url, ds);
          }
        }
        None => bail!("no open file found for DidChangeTextDocument"),
      },
    }
    Ok(())
  })?;
  n = helpers::try_notif::<lsp_types::notification::DidOpenTextDocument, _>(n, |params| {
    if let Mode::NoRoot(open_files) = &mut st.mode {
      let url = params.text_document.uri;
      let text = params.text_document.text;
      let path = convert::url_to_path_id(&st.cx.fs, &mut st.cx.paths, &url)?;
      let ds = convert::diagnostics(
        st.analysis.get_one(&text),
        st.cx.options.diagnostics.more_info_hint.0,
      );
      st.cx.send_diagnostics(url, ds);
      open_files.insert(path, text);
    }
    Ok(())
  })?;
  n = helpers::try_notif::<lsp_types::notification::DidSaveTextDocument, _>(n, |params| {
    match &mut st.mode {
      Mode::Root(root) => {
        if st.cx.registered_for_watched_files {
          log::warn!("ignoring DidSaveTextDocument since we registered for watched file events");
        } else {
          root.input = st.cx.get_input(&root.path);
          diagnostics::try_publish(st);
        }
      }
      Mode::NoRoot(open_files) => {
        if params.text.is_some() {
          log::warn!("got text for DidSaveTextDocument");
        }
        let url = params.text_document.uri;
        let path = convert::url_to_path_id(&st.cx.fs, &mut st.cx.paths, &url)?;
        match open_files.get(&path) {
          Some(text) => {
            let ds = convert::diagnostics(
              st.analysis.get_one(text),
              st.cx.options.diagnostics.more_info_hint.0,
            );
            st.cx.send_diagnostics(url, ds);
          }
          None => bail!("no open file found for DidSaveTextDocument"),
        }
      }
    }
    Ok(())
  })?;
  n = helpers::try_notif::<lsp_types::notification::DidCloseTextDocument, _>(n, |params| {
    if let Mode::NoRoot(open_files) = &mut st.mode {
      let url = params.text_document.uri;
      let path = convert::url_to_path_id(&st.cx.fs, &mut st.cx.paths, &url)?;
      open_files.remove(&path);
      st.cx.send_diagnostics(url, Vec::new());
    }
    Ok(())
  })?;
  ControlFlow::Continue(n)
}
