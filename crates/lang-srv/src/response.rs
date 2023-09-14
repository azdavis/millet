//! Respond to requests.

use crate::{convert, cx, state::St};
use lsp_server::Response;

pub(crate) fn handle(st: &mut St, res: Response) {
  log::info!("got response: {res:?}");
  let Some(data) = st.cx.req_queue.outgoing.complete(res.id.clone()) else {
    log::warn!("received response for non-queued request: {res:?}");
    return;
  };
  let Some(code) = data else {
    log::info!("no error code associated with this request");
    return;
  };
  let Some(val) = res.result else {
    log::info!("user did not click to look at the error URL");
    return;
  };
  let item = match serde_json::from_value::<lsp_types::MessageActionItem>(val) {
    Ok(x) => x,
    Err(e) => {
      log::error!("registered an error code, but got no message action item: {e}");
      return;
    }
  };
  if item.title != cx::HELP_FIX {
    log::warn!("unknown item.title: {}", item.title);
    return;
  }
  st.cx.send_request::<lsp_types::request::ShowDocument>(
    lsp_types::ShowDocumentParams {
      uri: convert::error_url(code),
      external: Some(true),
      take_focus: Some(true),
      selection: None,
    },
    None,
  );
}
