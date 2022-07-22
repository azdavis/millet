use fast_hash::FxHashMap;
use statics::{basis, Info, Syms};
use syntax::{ast::AstNode as _, SyntaxKind, SyntaxNode};

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
    get_std_basis(std::iter::empty())
  }

  /// The full standard basis, as documented in the public SML basis library docs.
  pub fn full() -> Self {
    elapsed::log("StdBasis::full", || {
      get_std_basis(
        std::iter::empty()
          .chain(sml_libs::std_basis::FILES)
          .chain(sml_libs::std_basis_extra::FILES)
          .chain(sml_libs::sml_nj::FILES)
          .copied(),
      )
    })
  }

  pub(crate) fn syms(&self) -> &Syms {
    &self.syms
  }

  pub(crate) fn basis(&self) -> &basis::Basis {
    &self.basis
  }

  /// Look up a std basis file's info.
  pub(crate) fn get_info(&self, s: &str) -> Option<&Info> {
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
          .map(|line| {
            if line == STREAM_IO_REGULAR {
              STREAM_IO_TEXT
            } else {
              line
            }
          })
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
      let lexed = lex::get(contents);
      if let Some(e) = lexed.errors.first() {
        panic!("{name}: lex error: {}", e.display());
      }
      let parsed = parse::get(&lexed.tokens, &mut parse::parser::STD_BASIS.clone());
      if let Some(e) = parsed.errors.first() {
        panic!("{name}: parse error: {}", e.display());
      }
      let mut low = lower::get(&parsed.root);
      if let Some(e) = low.errors.first() {
        panic!("{name}: lower error: {}", e.display());
      }
      ty_var_scope::get(&mut low.arenas, low.root);
      let comment_map: FxHashMap<hir::Idx, _> = low
        .arenas
        .spec
        .iter()
        .filter_map(|(idx, _)| {
          let ptr = low
            .ptrs
            .hir_to_ast(idx.into())
            .expect("no syntax ptr for spec");
          let node = ptr.to_node(parsed.root.syntax());
          let com = maybe_get_spec_comment(&node)?;
          Some((idx.into(), com))
        })
        .collect();
      let mode = statics::Mode::StdBasis(name, comment_map);
      let checked = statics::get(&mut syms, &basis, mode, &low.arenas, low.root);
      basis.append(checked.basis);
      if let Some(e) = checked.errors.first() {
        panic!(
          "{name}: statics error: {}",
          e.display(&syms, checked.info.meta_vars(), config::ErrorLines::One)
        );
      }
      (name, checked.info)
    })
    .collect();
  StdBasis { syms, basis, info }
}

/// NOTE: this is hard-coded for specs in a declaration file. maybe we could make `(*! ... !*)`
/// comments "doc comments" more generally?
fn maybe_get_spec_comment(node: &SyntaxNode) -> Option<String> {
  let com = node
    .parent()?
    .prev_sibling_or_token()?
    .as_token()?
    .prev_sibling_or_token()?;
  let com = com.as_token()?;
  if com.kind() != SyntaxKind::BlockComment {
    return None;
  }
  let mut lines: Vec<_> = com.text().lines().map(str::trim).collect();
  if lines.remove(0) != "(*!" || lines.pop()? != "!*)" {
    return None;
  }
  Some(lines.join(" "))
}
