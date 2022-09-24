//! A language server for Standard ML.

mod state;

fn run(conn: lsp_server::Connection, init: lsp_types::InitializeParams) -> anyhow::Result<()> {
  log::info!("startup main loop: {init:#?}");
  let mut state = state::State::new(init, conn.sender.clone());
  for msg in conn.receiver.iter() {
    match msg {
      lsp_server::Message::Request(req) => {
        if conn.handle_shutdown(&req)? {
          log::info!("shutdown main loop");
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

fn main() -> anyhow::Result<()> {
  env_logger::try_init()?;
  log::info!("startup millet lsp server");
  let (connection, io_threads) = lsp_server::Connection::stdio();
  let params = connection.initialize(serde_json::to_value(&state::capabilities())?)?;
  run(connection, serde_json::from_value(params)?)?;
  io_threads.join()?;
  log::info!("shutdown millet lsp server");
  Ok(())
}
