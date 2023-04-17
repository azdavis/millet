//! Handle requests.

use crate::state::{Mode, St};
use crate::{convert, helpers};
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

/// TODO replace with constant from lsp types after it's updated with 3.17
const REQUEST_FAILED: i32 = -32803;

#[allow(clippy::too_many_lines)]
fn go(st: &mut St, mut r: Request) -> ControlFlow<Result<()>, Request> {
  r = helpers::try_req::<lsp_types::request::HoverRequest, _>(r, |id, params| {
    let params = params.text_document_position_params;
    let pos = convert::text_doc_pos_params(&st.cx.fs, &mut st.cx.store, &params)?;
    let res = st.analysis.get_md(pos, st.cx.options.show_token_hover).map(|(value, range)| {
      lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
          kind: lsp_types::MarkupKind::Markdown,
          value,
        }),
        range: Some(convert::lsp_range(range)),
      }
    });
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::GotoDefinition, _>(r, |id, params| {
    let params = params.text_document_position_params;
    let pos = convert::text_doc_pos_params(&st.cx.fs, &mut st.cx.store, &params)?;
    let res: Vec<_> = st
      .analysis
      .get_defs(pos)
      .into_iter()
      .flatten()
      .filter_map(|range| convert::lsp_location(&st.cx.store, range))
      .collect();
    st.cx.send_response(Response::new_ok(id, lsp_types::GotoDefinitionResponse::Array(res)));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::GotoTypeDefinition, _>(r, |id, params| {
    let params = params.text_document_position_params;
    let pos = convert::text_doc_pos_params(&st.cx.fs, &mut st.cx.store, &params)?;
    let locs: Vec<_> = st
      .analysis
      .get_ty_defs(pos)
      .into_iter()
      .flatten()
      .filter_map(|range| convert::lsp_location(&st.cx.store, range))
      .collect();
    let res = (!locs.is_empty()).then_some(lsp_types::GotoDefinitionResponse::Array(locs));
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::CodeActionRequest, _>(r, |id, params| {
    let url = params.text_document.uri;
    let path = convert::url_to_path_id(&st.cx.fs, &mut st.cx.store, &url)?;
    let range = convert::analysis_range(params.range);
    let mut actions = Vec::<lsp_types::CodeActionOrCommand>::new();
    if let Some((range, new_text)) = st.analysis.fill_case(path.wrap(range.start)) {
      actions.push(convert::quick_fix("Fill case".to_owned(), url, range, new_text));
    }
    st.cx.send_response(Response::new_ok(id, actions));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::Formatting, _>(r, |id, params| {
    let url = params.text_document.uri;
    let path = convert::url_to_path_id(&st.cx.fs, &mut st.cx.store, &url)?;
    let res = match &st.mode {
      Mode::Root(_) => match st.analysis.format(path, params.options.tab_size) {
        Ok((new_text, end)) => {
          let edits = vec![lsp_types::TextEdit {
            range: lsp_types::Range {
              start: lsp_types::Position { line: 0, character: 0 },
              end: convert::lsp_position(end),
            },
            new_text,
          }];
          Response::new_ok(id, edits)
        }
        Err(e) => match e {
          analysis::FormatError::NoFile
          | analysis::FormatError::Disabled
          | analysis::FormatError::NaiveFmt(_)
          | analysis::FormatError::Smlfmt(analysis::SmlfmtError::Unsuccessful(_)) => {
            Response::new_ok(id, None::<()>)
          }
          analysis::FormatError::Smlfmt(e) => {
            Response::new_err(id, REQUEST_FAILED, format!("{e:#}"))
          }
        },
      },
      Mode::NoRoot(_) => Response::new_ok(id, None::<()>),
    };
    st.cx.send_response(res);
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::DocumentSymbolRequest, _>(r, |id, params| {
    let url = params.text_document.uri;
    let path = convert::url_to_path_id(&st.cx.fs, &mut st.cx.store, &url)?;
    let res: Option<Vec<_>> = st
      .analysis
      .document_symbols(path)
      .map(|xs| xs.into_iter().map(convert::document_symbol).collect());
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::References, _>(r, |id, params| {
    let params = params.text_document_position;
    let pos = convert::text_doc_pos_params(&st.cx.fs, &mut st.cx.store, &params)?;
    let res: Option<Vec<_>> = st.analysis.find_all_references(pos).map(|locs| {
      locs.into_iter().filter_map(|loc| convert::lsp_location(&st.cx.store, loc)).collect()
    });
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::Completion, _>(r, |id, params| {
    let params = params.text_document_position;
    let pos = convert::text_doc_pos_params(&st.cx.fs, &mut st.cx.store, &params)?;
    let res: Option<Vec<_>> =
      st.analysis.completions(pos).map(|cs| cs.into_iter().map(convert::completion_item).collect());
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  r = helpers::try_req::<lsp_types::request::InlayHintRequest, _>(r, |id, params| {
    let url = params.text_document.uri;
    let path = convert::url_to_path_id(&st.cx.fs, &mut st.cx.store, &url)?;
    let range = convert::analysis_range(params.range);
    let res: Vec<_> = st
      .analysis
      .inlay_hints(path.wrap(range))
      .into_iter()
      .flat_map(|xs| xs.into_iter().map(convert::inlay_hint))
      .collect();
    st.cx.send_response(Response::new_ok(id, res));
    Ok(())
  })?;
  ControlFlow::Continue(r)
}
