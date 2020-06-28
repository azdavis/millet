//! The core of the server logic.

use crate::serde::{Request, RequestParams, Response, ResponseSuccess};
use lsp_types::{InitializeResult, Url};

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
        Ok(ResponseSuccess::Initialize(InitializeResult::default()))
      }
    };
    Some(Response {
      id: Some(req.id),
      res,
    })
  }
}
