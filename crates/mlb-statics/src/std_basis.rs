use crate::{add_all_doc_comments, SourceFileSyntax};
use fast_hash::FxHashMap;
use once_cell::sync::Lazy;
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
  #[must_use]
  pub fn minimal() -> Self {
    MINIMAL.clone()
  }

  /// The full standard basis, as documented in the public SML basis library docs.
  #[must_use]
  pub fn full() -> Self {
    FULL.clone()
  }

  /// Returns the symbols for this.
  #[must_use]
  pub fn syms(&self) -> &Syms {
    &self.syms
  }

  /// Returns the basis for this.
  #[must_use]
  pub fn basis(&self) -> &basis::Basis {
    &self.basis
  }

  /// Look up a std basis file's info.
  #[must_use]
  pub fn get_info(&self, s: &str) -> Option<&Info> {
    self.info.get(s)
  }
}

static MINIMAL: Lazy<StdBasis> = Lazy::new(|| get_std_basis(std::iter::empty()));

static FULL: Lazy<StdBasis> = Lazy::new(|| {
  get_std_basis(
    std::iter::empty()
      .chain(sml_libs::primitive::FILES)
      .chain(sml_libs::std_basis::FILES)
      .chain(sml_libs::std_basis_extra::FILES)
      .chain(sml_libs::smlnj_lib::FILES)
      .chain(sml_libs::sml_of_nj::FILES)
      .chain(sml_libs::mlton::FILES)
      .copied(),
  )
});

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
      if name == "std_basis/imperative-io.sml" {
        let mut lines: Vec<_> = contents
          .lines()
          .skip(5)
          .map(|line| if line == STREAM_IO_REGULAR { STREAM_IO_TEXT } else { line })
          .collect();
        assert_eq!(lines.pop().unwrap(), "end");
        imperative_io_hack = Some(lines.join("\n"));
      }
      let owned_contents: String;
      if name == "std_basis/text-io.sml" {
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
      let mut fix_env = sml_parse::parser::STD_BASIS.clone();
      let started = SourceFileSyntax::new(&mut fix_env, contents);
      if let Some(e) = started.lex_errors.first() {
        panic!("{name}: lex error: {}", e.display());
      }
      if let Some(e) = started.parse.errors.first() {
        panic!("{name}: parse error: {}", e.display());
      }
      if let Some(e) = started.lower.errors.first() {
        panic!("{name}: lower error: {}", e.display());
      }
      let mode = sml_statics::Mode::BuiltinLib(name);
      let low = started.lower;
      let checked = sml_statics::get(&mut syms, &basis, mode, &low.arenas, low.root);
      basis.append(checked.basis);
      if let Some(e) = checked.errors.first() {
        let e = e.display(&syms, checked.info.meta_vars(), config::ErrorLines::One);
        panic!("{name}: statics error: {e}");
      }
      let mut info = checked.info;
      add_all_doc_comments(started.parse.root.syntax(), &low, &mut info);
      (name, info)
    })
    .collect();
  StdBasis { syms, basis, info }
}
