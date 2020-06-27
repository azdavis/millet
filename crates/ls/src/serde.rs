//! Types for messages to and from the server.

use lsp_types::InitializeParams;
use serde_json::{from_slice, from_value, Value};

#[derive(Debug)]
pub enum Id {
  Number(u64),
  String(String),
}

#[derive(Debug)]
pub enum RequestParams {
  Initialize(InitializeParams),
}

#[derive(Debug)]
pub struct Request {
  pub id: Id,
  params: RequestParams,
}

impl Request {
  pub fn try_parse(bs: &[u8]) -> Option<Self> {
    let mut val: Value = from_slice(bs).ok()?;
    if val.get("jsonrpc")?.as_str()? != "2.0" {
      return None;
    }
    let id = match std::mem::take(val.get_mut("id")?) {
      Value::Number(n) => Id::Number(n.as_u64()?),
      Value::String(s) => Id::String(s),
      _ => return None,
    };
    let params = std::mem::take(val.get_mut("params")?);
    let params = match val.get("method")?.as_str()? {
      "initialize" => RequestParams::Initialize(from_value(params).ok()?),
      _ => return None,
    };
    Some(Self { id, params })
  }
}

pub struct Response {}
