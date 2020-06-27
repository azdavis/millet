//! The main handler.

use crate::serde::{Request, Response};

pub struct State {}

impl State {
  pub fn new() -> Self {
    Self {}
  }
}

pub fn get(st: &mut State, req: Request) -> Response {
  todo!()
}
