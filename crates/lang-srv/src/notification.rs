//! Handle notifications.

use crate::state::{Mode, St};
use crate::{convert, diagnostics, helpers};
use anyhow::{bail, Result};
use lsp_server::Notification;
use std::ops::ControlFlow;

pub(crate) fn handle(st: &mut St, notif: Notification) {
  log::info!("got notification: {notif:?}");
  match go(st, notif) {
    ControlFlow::Break(Ok(())) => {}
    ControlFlow::Break(Err(e)) => log::error!("couldn't handle notification: {e}"),
    ControlFlow::Continue(notif) => log::warn!("unhandled notification: {notif:?}"),
  }
}

fn go(st: &mut St, mut n: Notification) -> ControlFlow<Result<()>, Notification> {
  n = helpers::try_notif::<lsp_types::notification::DidChangeWatchedFiles, _>(n, |_| {
    match &mut st.mode {
      Mode::Root(root) => {
        root.input = st.cx.try_get_input(&root.path, &mut st.has_diagnostics);
        diagnostics::try_publish(st);
      }
      Mode::NoRoot(_) => log::warn!("ignoring DidChangeWatchedFiles with NoRoot"),
    }
    Ok(())
  })?;
  n = helpers::try_notif::<lsp_types::notification::DidChangeTextDocument, _>(n, |params| {
    let url = params.text_document.uri;
    let path = convert::url_to_path_id(&st.cx.file_system, &mut st.cx.store, &url)?;
    match &mut st.mode {
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
        if st.cx.options.diagnostics_on_change {
          diagnostics::try_publish(st);
        } else if st.cx.options.format.is_some() {
          // TODO this is expensive, but currently necessary to make formatting work. can we
          // make it just do it for formatting (i.e. syntax) only (no statics)?
          st.analysis.get_many(input);
        }
      }
      Mode::NoRoot(open_files) => match open_files.get_mut(&path) {
        Some(text) => {
          helpers::apply_changes(text, params.content_changes);
          if st.cx.options.diagnostics_on_change {
            let ds = convert::diagnostics(
              st.analysis.get_one(text),
              st.cx.options.diagnostics_more_info_hint,
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
      let path = convert::url_to_path_id(&st.cx.file_system, &mut st.cx.store, &url)?;
      let ds =
        convert::diagnostics(st.analysis.get_one(&text), st.cx.options.diagnostics_more_info_hint);
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
          root.input = st.cx.try_get_input(&root.path, &mut st.has_diagnostics);
          diagnostics::try_publish(st);
        }
      }
      Mode::NoRoot(open_files) => {
        if params.text.is_some() {
          log::warn!("got text for DidSaveTextDocument");
        }
        let url = params.text_document.uri;
        let path = convert::url_to_path_id(&st.cx.file_system, &mut st.cx.store, &url)?;
        match open_files.get(&path) {
          Some(text) => {
            let ds = convert::diagnostics(
              st.analysis.get_one(text),
              st.cx.options.diagnostics_more_info_hint,
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
      let path = convert::url_to_path_id(&st.cx.file_system, &mut st.cx.store, &url)?;
      open_files.remove(&path);
      st.cx.send_diagnostics(url, Vec::new());
    }
    Ok(())
  })?;
  ControlFlow::Continue(n)
}
