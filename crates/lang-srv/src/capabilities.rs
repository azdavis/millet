//! See [`get`].

/// TODO set to true, then remove
const COMPLETIONS: bool = false;

/// TODO set to true, then remove
const INLAY_HINTS: bool = false;

/// Returns the capabilities of the server.
pub(crate) fn get() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
      lsp_types::TextDocumentSyncOptions {
        open_close: Some(false),
        change: Some(lsp_types::TextDocumentSyncKind::INCREMENTAL),
        will_save: Some(false),
        will_save_wait_until: Some(false),
        save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(lsp_types::SaveOptions {
          include_text: Some(false),
        })),
      },
    )),
    hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
    definition_provider: Some(lsp_types::OneOf::Left(true)),
    type_definition_provider: Some(lsp_types::TypeDefinitionProviderCapability::Simple(true)),
    code_action_provider: Some(lsp_types::CodeActionProviderCapability::Simple(true)),
    document_formatting_provider: Some(lsp_types::OneOf::Left(true)),
    document_symbol_provider: Some(lsp_types::OneOf::Left(true)),
    references_provider: Some(lsp_types::OneOf::Left(true)),
    completion_provider: COMPLETIONS.then(|| lsp_types::CompletionOptions {
      trigger_characters: Some(vec![".".to_owned()]),
      ..lsp_types::CompletionOptions::default()
    }),
    inlay_hint_provider: INLAY_HINTS.then_some(lsp_types::OneOf::Left(true)),
    ..Default::default()
  }
}
