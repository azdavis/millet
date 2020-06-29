//! Types for messages to and from the server.

use lsp_types::{
  DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
  DidSaveTextDocumentParams, InitializeParams, InitializeResult, NumberOrString,
  PublishDiagnosticsParams,
};
use serde::de::DeserializeOwned;
use serde_json::{from_slice, from_value, json, to_value, to_vec, Error, Map, Value};

const JSON_RPC_VERSION: &str = "2.0";

pub enum RequestParams {
  Initialize(InitializeParams),
  Shutdown,
}

pub struct Request {
  pub id: NumberOrString,
  pub params: RequestParams,
}

pub enum IncomingNotification {
  Initialized,
  Exit,
  TextDocOpen(DidOpenTextDocumentParams),
  TextDocChange(DidChangeTextDocumentParams),
  TextDocSave(DidSaveTextDocumentParams),
  TextDocClose(DidCloseTextDocumentParams),
}

pub enum Incoming {
  Request(Request),
  Notification(IncomingNotification),
}

impl Incoming {
  fn request(id: NumberOrString, params: RequestParams) -> Self {
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
      "initialized" => Incoming::Notification(IncomingNotification::Initialized),
      "shutdown" => Incoming::request(get_id(&mut val)?, RequestParams::Shutdown),
      "exit" => Incoming::Notification(IncomingNotification::Exit),
      "textDocument/didOpen" => {
        Incoming::Notification(IncomingNotification::TextDocOpen(get_params(&mut val)?))
      }
      "textDocument/didClose" => {
        Incoming::Notification(IncomingNotification::TextDocClose(get_params(&mut val)?))
      }
      "textDocument/didChange" => {
        Incoming::Notification(IncomingNotification::TextDocChange(get_params(&mut val)?))
      }
      "textDocument/didSave" => {
        Incoming::Notification(IncomingNotification::TextDocSave(get_params(&mut val)?))
      }
      _ => return None,
    };
    Some(ret)
  }
}

fn get_id(val: &mut Value) -> Option<NumberOrString> {
  from_value(std::mem::take(val.get_mut("id")?)).ok()
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
  pub id: Option<NumberOrString>,
  pub res: Result<ResponseSuccess, ResponseError>,
}

impl Response {
  fn into_vec(self) -> Result<Vec<u8>, Error> {
    let mut map = Map::with_capacity(3);
    map.insert("jsonrpc".to_owned(), JSON_RPC_VERSION.into());
    map.insert("id".to_owned(), to_value(&self.id)?);
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

pub enum OutgoingNotification {
  PublishDiagnostics(PublishDiagnosticsParams),
}

impl OutgoingNotification {
  fn into_vec(self) -> Result<Vec<u8>, Error> {
    let mut map = Map::with_capacity(3);
    map.insert("jsonrpc".to_owned(), JSON_RPC_VERSION.into());
    match self {
      Self::PublishDiagnostics(params) => {
        map.insert(
          "method".to_owned(),
          "textDocument/publishDiagnostics".into(),
        );
        map.insert("params".to_owned(), to_value(&params)?);
      }
    }
    to_vec(&Value::Object(map))
  }
}

pub enum Outgoing {
  Response(Response),
  Notification(OutgoingNotification),
}

impl Outgoing {
  pub fn into_vec(self) -> Result<Vec<u8>, Error> {
    match self {
      Self::Response(res) => res.into_vec(),
      Self::Notification(notif) => notif.into_vec(),
    }
  }
}
