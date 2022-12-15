//! A language server for Standard ML.

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]
#![allow(clippy::single_match_else)]

mod capabilities;
mod helpers;
mod state;

fn run_inner(
  conn: &lsp_server::Connection,
  init: lsp_types::InitializeParams,
) -> anyhow::Result<()> {
  log::info!("start up main loop: {init:#?}");
  let mut state = state::State::new(init, conn.sender.clone());
  for msg in conn.receiver.iter() {
    match msg {
      lsp_server::Message::Request(req) => {
        if conn.handle_shutdown(&req)? {
          log::info!("shut down main loop");
          return Ok(());
        }
        state.handle_request(req);
      }
      lsp_server::Message::Response(res) => state.handle_response(res),
      lsp_server::Message::Notification(notif) => state.handle_notification(notif),
    }
  }
  Ok(())
}

/// Run the language server over stdio.
///
/// # Errors
///
/// If the language server encountered an error.
pub fn run() -> anyhow::Result<()> {
  let (connection, io_threads) = lsp_server::Connection::stdio();
  let params = connection.initialize(serde_json::to_value(&capabilities::get())?)?;
  run_inner(&connection, serde_json::from_value(params)?)?;
  io_threads.join()?;
  Ok(())
}
