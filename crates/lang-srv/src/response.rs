//! Respond to requests.

use crate::cx::LEARN_MORE;
use crate::helpers;
use crate::state::St;
use lsp_server::Response;

pub(crate) fn handle(st: &mut St, res: Response) {
  log::info!("got response: {res:?}");
  let data = match st.cx.req_queue.outgoing.complete(res.id.clone()) {
    Some(x) => x,
    None => {
      log::warn!("received response for non-queued request: {res:?}");
      return;
    }
  };
  let code = match data {
    Some(x) => x,
    None => {
      log::info!("no error code associated with this request");
      return;
    }
  };
  let val = match res.result {
    Some(x) => x,
    None => {
      log::info!("user did not click to look at the error URL");
      return;
    }
  };
  let item = match serde_json::from_value::<lsp_types::MessageActionItem>(val) {
    Ok(x) => x,
    Err(e) => {
      log::error!("registered an error code, but got no message action item: {e}");
      return;
    }
  };
  if item.title != LEARN_MORE {
    log::warn!("unknown item.title: {}", item.title);
    return;
  }
  st.cx.send_request::<lsp_types::request::ShowDocument>(
    lsp_types::ShowDocumentParams {
      uri: helpers::error_url(code),
      external: Some(true),
      take_focus: Some(true),
      selection: None,
    },
    None,
  );
}
