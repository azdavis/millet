//! Handle requests.

use crate::{helpers, state::St};
use anyhow::Result;
use lsp_server::{Request, Response};
use std::ops::ControlFlow;

pub(crate) fn handle(st: &mut St, req: Request) {
  log::info!("got request: {req:?}");
  st.cx.req_queue.incoming.register(req.id.clone(), ());
  match go(st, req) {
    ControlFlow::Break(Ok(())) => {}
    ControlFlow::Break(Err(e)) => log::error!("couldn't handle request: {e}"),
    ControlFlow::Continue(req) => log::warn!("unhandled request: {req:?}"),
  }
}

fn go(st: &mut St, mut r: Request) -> ControlFlow<Result<()>, Request> {
  r = helpers::try_req::<lsp_types::request::HoverRequest, _>(r, |id, params| {
    let params = params.text_document_position_params;
    let pos = helpers::text_doc_pos_params(&st.cx.file_system, &mut st.cx.store, &params)?;
    let res = st.analysis.get_md(pos, st.cx.options.show_token_hover).map(|(value, range)| {
      lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
          kind: lsp_types::MarkupKind::Markdown,
          value,
        }),
        range: Some(helpers::lsp_range(range)),
      }
    });
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::GotoDefinition, _>(r, |id, params| {
    let params = params.text_document_position_params;
    let pos = helpers::text_doc_pos_params(&st.cx.file_system, &mut st.cx.store, &params)?;
    let res = st.analysis.get_def(pos).and_then(|range| {
      helpers::lsp_location(&st.cx.store, range).map(lsp_types::GotoDefinitionResponse::Scalar)
    });
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::GotoTypeDefinition, _>(r, |id, params| {
    let params = params.text_document_position_params;
    let pos = helpers::text_doc_pos_params(&st.cx.file_system, &mut st.cx.store, &params)?;
    let locs: Vec<_> = st
      .analysis
      .get_ty_defs(pos)
      .into_iter()
      .flatten()
      .filter_map(|range| helpers::lsp_location(&st.cx.store, range))
      .collect();
    let res = (!locs.is_empty()).then_some(lsp_types::GotoDefinitionResponse::Array(locs));
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::CodeActionRequest, _>(r, |id, params| {
    let url = params.text_document.uri;
    let path = helpers::url_to_path_id(&st.cx.file_system, &mut st.cx.store, &url)?;
    let range = helpers::analysis_range(params.range);
    let mut actions = Vec::<lsp_types::CodeActionOrCommand>::new();
    if let Some((range, new_text)) = st.analysis.fill_case(path.wrap(range.start)) {
      actions.push(helpers::quick_fix("Fill case".to_owned(), url, range, new_text));
    }
    st.cx.send_response(Response::new_ok(id, actions));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::Formatting, _>(r, |id, params| {
    let url = params.text_document.uri;
    let path = helpers::url_to_path_id(&st.cx.file_system, &mut st.cx.store, &url)?;
    let res = st.analysis.format(path, params.options.tab_size).ok().map(|(new_text, end)| {
      vec![lsp_types::TextEdit {
        range: lsp_types::Range {
          start: lsp_types::Position { line: 0, character: 0 },
          end: helpers::lsp_position(end),
        },
        new_text,
      }]
    });
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::DocumentSymbolRequest, _>(r, |id, params| {
    let url = params.text_document.uri;
    let path = helpers::url_to_path_id(&st.cx.file_system, &mut st.cx.store, &url)?;
    let res: Option<Vec<_>> =
      st.analysis.symbols(path).map(|xs| xs.into_iter().map(helpers::symbol).collect());
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  ControlFlow::Continue(r)
}
