//! A language server for Standard ML.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(clippy::single_match_else)]
// TODO remove once rustfmt support lands
#![allow(clippy::manual_let_else)]

mod capabilities;
mod convert;
mod cx;
mod diagnostics;
mod helpers;
mod init;
mod notification;
mod request;
mod response;
mod state;

fn run_inner(
  conn: &lsp_server::Connection,
  init: lsp_types::InitializeParams,
) -> anyhow::Result<()> {
  log::info!("start up main loop: {init:#?}");
  let mut st = init::init(init, conn.sender.clone());
  for msg in &conn.receiver {
    match msg {
      lsp_server::Message::Request(req) => {
        if conn.handle_shutdown(&req)? {
          log::info!("shut down main loop");
          return Ok(());
        }
        request::handle(&mut st, req);
      }
      lsp_server::Message::Response(res) => response::handle(&mut st, res),
      lsp_server::Message::Notification(notif) => notification::handle(&mut st, notif),
    }
  }
  Ok(())
}

/// Runs the language server over stdio.
///
/// # Errors
///
/// If the language server encountered an error.
pub fn run_stdio() -> anyhow::Result<()> {
  let (connection, io_threads) = lsp_server::Connection::stdio();
  let params = connection.initialize(serde_json::to_value(capabilities::get())?)?;
  run_inner(&connection, serde_json::from_value(params)?)?;
  // if we don't drop this, then the join hangs
  drop(connection);
  io_threads.join()?;
  Ok(())
}
