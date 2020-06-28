//! The core of the server logic.

use crate::serde::{Request, RequestParams, Response, ResponseSuccess};
use lsp_types::{InitializeResult, ServerCapabilities, ServerInfo, Url};

pub struct State {
  root_uri: Option<Url>,
}

impl State {
  pub fn new() -> Self {
    Self { root_uri: None }
  }

  pub fn handle(&mut self, req: Request) -> Option<Response> {
    let res = match req.params {
      RequestParams::Initialize(params) => {
        let _ = params.process_id?;
        self.root_uri = params.root_uri;
        Ok(ResponseSuccess::Initialize(InitializeResult {
          capabilities: ServerCapabilities {
            hover_provider: Some(true),
            ..ServerCapabilities::default()
          },
          server_info: Some(ServerInfo {
            name: "millet-ls".to_owned(),
            version: Some(env!("CARGO_PKG_VERSION").to_owned()),
          }),
        }))
      }
    };
    Some(Response {
      id: Some(req.id),
      res,
    })
  }
}
