/// Returns the capabilities of the server.
pub(crate) fn get() -> lsp_types::ServerCapabilities {
  lsp_types::ServerCapabilities {
    text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Options(
      lsp_types::TextDocumentSyncOptions {
        open_close: Some(true),
        change: Some(lsp_types::TextDocumentSyncKind::INCREMENTAL),
        will_save: None,
        will_save_wait_until: None,
        save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(lsp_types::SaveOptions {
          include_text: None,
        })),
      },
    )),
    hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
    definition_provider: Some(lsp_types::OneOf::Left(true)),
    type_definition_provider: Some(lsp_types::TypeDefinitionProviderCapability::Simple(true)),
    code_action_provider: Some(lsp_types::CodeActionProviderCapability::Simple(true)),
    document_formatting_provider: Some(lsp_types::OneOf::Left(true)),
    ..Default::default()
  }
}
