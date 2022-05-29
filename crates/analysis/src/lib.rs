//! Analysis.

#![deny(missing_debug_implementations)]
#![deny(missing_docs)]
#![deny(rust_2018_idioms)]

use syntax::{ast::AstNode as _, rowan::TextRange};

type SyntaxNodePtr = syntax::ast::SyntaxNodePtr<syntax::SML>;

use url::Url;

/// An error.
#[derive(Debug)]
pub struct Error {
  /// The URL for the error.
  pub url: Url,
  /// The range of the error.
  pub range: TextRange,
  /// The message of the error.
  pub message: String,
}

/// Turn a sequence of files into a sequence of errors.
///
/// TODO should probably have this be more like a data structure that can support adding/removing
/// files and other queries other than errors. Then we could be more incremental.
pub fn get(files: Vec<(Url, String)>) -> Vec<Error> {
  let mut files: Vec<_> = files
    .into_iter()
    .map(|(url, contents)| {
      let lexed = lex::get(&contents);
      let parsed = parse::get(&lexed.tokens);
      let lowered = lower::get(&parsed.root);
      AnalyzedFile {
        url,
        lex_errors: lexed.errors,
        parsed,
        lowered,
        statics_errors: Vec::new(),
      }
    })
    .collect();
  let mut st = statics::Statics::default();
  for file in files.iter_mut() {
    statics::get(&mut st, &file.lowered.arenas, &file.lowered.top_decs);
    file.statics_errors = std::mem::take(&mut st.errors);
  }
  // can't use flat map because of borrowing file.url
  let mut ret = Vec::new();
  for file in files.into_iter() {
    ret.extend(
      std::iter::empty()
        .chain(file.lex_errors.into_iter().map(|err| Error {
          url: file.url.clone(),
          range: err.range,
          message: err.kind.to_string(),
        }))
        .chain(file.parsed.errors.into_iter().map(|err| Error {
          url: file.url.clone(),
          range: err.range,
          message: err.kind.to_string(),
        }))
        .chain(file.lowered.errors.into_iter().map(|err| Error {
          url: file.url.clone(),
          range: err.range,
          message: err.kind.to_string(),
        }))
        .chain(file.statics_errors.into_iter().filter_map(|err| {
          Some(Error {
            url: file.url.clone(),
            range: get_statics_ptr(&file.lowered.ptrs, err.idx())?
              .to_node(file.parsed.root.syntax())
              .text_range(),
            message: err.display(&st.syms).to_string(),
          })
        })),
    );
  }
  ret
}

fn get_statics_ptr(ptrs: &lower::Ptrs, idx: statics::Idx) -> Option<SyntaxNodePtr> {
  match idx {
    statics::Idx::Exp(idx) => ptrs.get_exp(idx),
    statics::Idx::Pat(idx) => ptrs.get_pat(idx),
    statics::Idx::Ty(idx) => ptrs.get_ty(idx),
    statics::Idx::Dec(idx) => ptrs.get_dec(idx),
    statics::Idx::StrDec(idx) => ptrs.get_str_dec(idx),
    statics::Idx::TopDec(idx) => ptrs.get_top_dec(idx),
  }
}

struct AnalyzedFile {
  url: Url,
  lex_errors: Vec<lex::Error>,
  parsed: parse::Parse,
  lowered: lower::Lower,
  statics_errors: Vec<statics::Error>,
}
