use lsp_server::Connection;
use lsp_types::{HoverProviderCapability, ServerCapabilities};

mod main_loop;
mod state;

fn main() -> anyhow::Result<()> {
  eprintln!("startup millet lsp server");
  let (connection, io_threads) = Connection::stdio();
  let server_capabilities = ServerCapabilities {
    // not actually true
    hover_provider: Some(HoverProviderCapability::Simple(true)),
    ..Default::default()
  };
  let params = connection.initialize(serde_json::to_value(&server_capabilities)?)?;
  main_loop::run(connection, serde_json::from_value(params)?)?;
  io_threads.join()?;
  eprintln!("shutdown millet lsp server");
  Ok(())
}
