//! Types for messages to and from the server.

use lsp_types::{
  DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
  DidSaveTextDocumentParams, InitializeParams, InitializeResult,
};
use serde::de::DeserializeOwned;
use serde_json::{from_slice, from_value, json, to_value, to_vec, Error, Map, Value};

const JSON_RPC_VERSION: &str = "2.0";

pub enum Id {
  Number(u64),
  String(String),
}

pub enum RequestParams {
  Initialize(InitializeParams),
  Shutdown,
}

pub struct Request {
  pub id: Id,
  pub params: RequestParams,
}

pub enum Notification {
  Initialized,
  Exit,
  TextDocOpen(DidOpenTextDocumentParams),
  TextDocChange(DidChangeTextDocumentParams),
  TextDocSave(DidSaveTextDocumentParams),
  TextDocClose(DidCloseTextDocumentParams),
}

pub enum Incoming {
  Request(Request),
  Notification(Notification),
}

impl Incoming {
  fn request(id: Id, params: RequestParams) -> Self {
    Self::Request(Request { id, params })
  }

  pub fn try_parse(bs: &[u8]) -> Option<Self> {
    let mut val: Value = from_slice(bs).ok()?;
    if val.get("jsonrpc")?.as_str()? != JSON_RPC_VERSION {
      return None;
    }
    let ret = match val.get("method")?.as_str()? {
      "initialize" => Incoming::request(
        get_id(&mut val)?,
        RequestParams::Initialize(get_params(&mut val)?),
      ),
      "initialized" => Incoming::Notification(Notification::Initialized),
      "shutdown" => Incoming::request(get_id(&mut val)?, RequestParams::Shutdown),
      "exit" => Incoming::Notification(Notification::Exit),
      "textDocument/didOpen" => {
        Incoming::Notification(Notification::TextDocOpen(get_params(&mut val)?))
      }
      "textDocument/didClose" => {
        Incoming::Notification(Notification::TextDocClose(get_params(&mut val)?))
      }
      "textDocument/didChange" => {
        Incoming::Notification(Notification::TextDocChange(get_params(&mut val)?))
      }
      "textDocument/didSave" => {
        Incoming::Notification(Notification::TextDocSave(get_params(&mut val)?))
      }
      _ => return None,
    };
    Some(ret)
  }
}

fn get_id(val: &mut Value) -> Option<Id> {
  match std::mem::take(val.get_mut("id")?) {
    Value::Number(n) => Some(Id::Number(n.as_u64()?)),
    Value::String(s) => Some(Id::String(s)),
    _ => None,
  }
}

fn get_params<T>(val: &mut Value) -> Option<T>
where
  T: DeserializeOwned,
{
  from_value(std::mem::take(val.get_mut("params")?)).ok()
}

pub enum ResponseSuccess {
  Initialize(InitializeResult),
  Null,
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
          ResponseSuccess::Null => Value::Null,
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
