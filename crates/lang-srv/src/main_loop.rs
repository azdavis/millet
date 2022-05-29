use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use lsp_types::{request::GotoDefinition, GotoDefinitionResponse, InitializeParams};

pub(crate) fn run(connection: Connection, _params: InitializeParams) -> anyhow::Result<()> {
  eprintln!("startup main loop");
  for msg in &connection.receiver {
    eprintln!("got msg: {:?}", msg);
    match msg {
      Message::Request(req) => {
        if connection.handle_shutdown(&req)? {
          return Ok(());
        }
        eprintln!("got request: {:?}", req);
        match cast::<GotoDefinition>(req) {
          Ok((id, params)) => {
            eprintln!("got GotoDefinition request {}: {:?}", id, params);
            // make a fake response
            let result = Some(GotoDefinitionResponse::Array(Vec::new()));
            let result = serde_json::to_value(&result).unwrap();
            let resp = Response {
              id,
              result: Some(result),
              error: None,
            };
            connection.sender.send(Message::Response(resp))?;
            continue;
          }
          Err(err @ ExtractError::JsonError { .. }) => {
            eprintln!("{:?}", err);
            continue;
          }
          // use the req for more
          Err(ExtractError::MethodMismatch(req)) => req,
        };
      }
      Message::Response(resp) => {
        eprintln!("got response: {:?}", resp);
      }
      Message::Notification(not) => {
        eprintln!("got notification: {:?}", not);
      }
    }
  }
  Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
  R: lsp_types::request::Request,
  R::Params: serde::de::DeserializeOwned,
{
  req.extract(R::METHOD)
}
