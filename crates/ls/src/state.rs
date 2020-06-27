//! The core of the server logic.

use crate::serde::{Request, Response};

pub struct State {}

impl State {
  pub fn new() -> Self {
    Self {}
  }

  pub fn handle(&mut self, req: Request) -> Response {
    todo!()
  }
}
