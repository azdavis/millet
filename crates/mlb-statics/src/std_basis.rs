use crate::{doc_comment, start_source_file};
use fast_hash::FxHashMap;
use sml_statics::{basis, Info, Syms};
use sml_syntax::ast::AstNode as _;

/// A standard basis.
#[derive(Debug, Clone)]
pub struct StdBasis {
  syms: Syms,
  basis: basis::Basis,
  info: FxHashMap<&'static str, Info>,
}

impl StdBasis {
  /// The minimal standard basis. Only includes fundamental top-level definitions like `int`,
  /// `real`, `ref`, `<`, etc.
  pub fn minimal() -> Self {
    get_std_basis(sml_libs::primitive::FILES.iter().copied())
  }

  /// The full standard basis, as documented in the public SML basis library docs.
  pub fn full() -> Self {
    get_std_basis(
      std::iter::empty()
        .chain(sml_libs::primitive::FILES)
        .chain(sml_libs::std_basis::FILES)
        .chain(sml_libs::std_basis_extra::FILES)
        .chain(sml_libs::sml_nj::FILES)
        .copied(),
    )
  }

  /// Returns the symbols for this.
  pub fn syms(&self) -> &Syms {
    &self.syms
  }

  /// Returns the basis for this.
  pub fn basis(&self) -> &basis::Basis {
    &self.basis
  }

  /// Look up a std basis file's info.
  pub fn get_info(&self, s: &str) -> Option<&Info> {
    self.info.get(s)
  }
}

const STREAM_IO_REGULAR: &str = "  structure StreamIO : STREAM_IO";
const STREAM_IO_TEXT: &str = r#"  structure StreamIO : TEXT_STREAM_IO
    where type reader = TextPrimIO.reader
    where type writer = TextPrimIO.writer
    where type pos = TextPrimIO.pos
"#;
const INCLUDE_IMPERATIVE_IO_HACK: &str = "  include IMPERATIVE_IO_HACK";

fn get_std_basis<I>(files: I) -> StdBasis
where
  I: Iterator<Item = (&'static str, &'static str)>,
{
  let (mut syms, mut basis) = basis::minimal();
  let mut imperative_io_hack = None::<String>;
  let info: FxHashMap<_, _> = files
    .map(|(name, mut contents)| {
      if name == "imperative-io.sml" {
        let mut lines: Vec<_> = contents
          .lines()
          .skip(5)
          .map(|line| if line == STREAM_IO_REGULAR { STREAM_IO_TEXT } else { line })
          .collect();
        assert_eq!(lines.pop().unwrap(), "end");
        imperative_io_hack = Some(lines.join("\n"));
      }
      let owned_contents: String;
      if name == "text-io.sml" {
        let lines: Vec<_> = contents
          .lines()
          .map(|line| {
            if line == INCLUDE_IMPERATIVE_IO_HACK {
              imperative_io_hack.as_deref().unwrap()
            } else {
              line
            }
          })
          .collect();
        owned_contents = lines.join("\n");
        contents = &owned_contents;
      }
      let mut fix_env = crate::STD_BASIS_FIX_ENV.clone();
      let (lex_errors, parsed, low) = start_source_file(contents, &mut fix_env);
      if let Some(e) = lex_errors.first() {
        panic!("{name}: lex error: {}", e.display());
      }
      if let Some(e) = parsed.errors.first() {
        panic!("{name}: parse error: {}", e.display());
      }
      if let Some(e) = low.errors.first() {
        panic!("{name}: lower error: {}", e.display());
      }
      let mode = sml_statics::Mode::StdBasis(name);
      let checked = sml_statics::get(&mut syms, &basis, mode, &low.arenas, low.root);
      basis.append(checked.basis);
      if let Some(e) = checked.errors.first() {
        let e = e.display(&syms, checked.info.meta_vars(), config::ErrorLines::One);
        panic!("{name}: statics error: {e}");
      }
      let mut info = checked.info;
      doc_comment::get(parsed.root.syntax(), &low, &mut info);
      (name, info)
    })
    .collect();
  StdBasis { syms, basis, info }
}
