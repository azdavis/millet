use crate::state::State;
use lsp_server::{Connection, Message};

pub(crate) fn run(conn: Connection, init: lsp_types::InitializeParams) -> anyhow::Result<()> {
  log::info!("startup main loop: {init:#?}");
  let root = match &init.root_uri {
    Some(x) => x.clone(),
    None => {
      log::info!("no root url");
      return Ok(());
    }
  };
  let mut state = State::new(root, conn.sender.clone());
  for msg in conn.receiver.iter() {
    match msg {
      Message::Request(req) => {
        if conn.handle_shutdown(&req)? {
          log::info!("shutdown main loop");
          return Ok(());
        }
        state.handle_request(req);
      }
      Message::Response(res) => state.handle_response(res),
      Message::Notification(notif) => state.handle_notification(notif),
    }
  }
  Ok(())
}
