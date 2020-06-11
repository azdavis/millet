//! Conversion from library error types to codespan Diagnostics.

use crate::source::SourceFileId;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use millet_core::error::Error;
use millet_core::loc::Located;

pub fn new(file_id: SourceFileId, err: Located<Error>) -> Diagnostic<SourceFileId> {
  Diagnostic::error()
    .with_message(err.val.to_string())
    .with_labels(vec![Label::primary(file_id, err.loc)])
}
