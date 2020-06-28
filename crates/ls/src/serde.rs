//! Types for messages to and from the server.

use lsp_types::{InitializeParams, InitializeResult};
use serde_json::{from_slice, from_value, json, to_value, to_vec, Error, Map, Value};

const JSON_RPC_VERSION: &str = "2.0";

pub enum Id {
  Number(u64),
  String(String),
}

pub enum RequestParams {
  Initialize(InitializeParams),
}

pub struct Request {
  pub id: Id,
  pub params: RequestParams,
}

impl Request {
  pub fn try_parse(bs: &[u8]) -> Option<Self> {
    let mut val: Value = from_slice(bs).ok()?;
    if val.get("jsonrpc")?.as_str()? != JSON_RPC_VERSION {
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

pub enum ResponseSuccess {
  Initialize(InitializeResult),
}

#[allow(unused)]
pub enum ErrorCode {
  ParseError = -32700,
  InvalidRequest = -32600,
  MethodNotFound = -32601,
  InvalidParams = -32602,
  InternalError = -32603,
  ServerErrorStart = -32099,
  ServerErrorEnd = -32000,
  ServerNotInitialized = -32002,
  UnknownErrorCode = -32001,
  RequestCancelled = -32800,
  ContentModified = -32801,
}

pub struct ResponseError {
  pub code: ErrorCode,
  pub message: String,
}

pub struct Response {
  pub id: Option<Id>,
  pub res: Result<ResponseSuccess, ResponseError>,
}

impl Response {
  pub fn into_vec(self) -> Result<Vec<u8>, Error> {
    let id = match self.id {
      None => Value::Null,
      Some(Id::Number(n)) => Value::Number(n.into()),
      Some(Id::String(s)) => Value::String(s),
    };
    let mut map = Map::with_capacity(3);
    map.insert("jsonrpc".to_owned(), JSON_RPC_VERSION.into());
    map.insert("id".to_owned(), id);
    let (key, val) = match self.res {
      Ok(good) => (
        "result",
        match good {
          ResponseSuccess::Initialize(x) => to_value(x)?,
        },
      ),
      Err(bad) => (
        "error",
        json!({"code": bad.code as i32, "message": bad.message}),
      ),
    };
    map.insert(key.to_owned(), val);
    to_vec(&Value::Object(map))
  }
}
