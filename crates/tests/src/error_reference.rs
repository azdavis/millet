use crate::check::{check_with_std_basis, fail_with_std_basis};
use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag};

const SML: &str = "sml";

#[test]
fn all() {
  let errors = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
    .parent()
    .unwrap()
    .parent()
    .unwrap()
    .join("docs")
    .join("errors.md");
  let errors = std::fs::read_to_string(&errors).unwrap();
  let mut options = Options::empty();
  options.insert(Options::ENABLE_TABLES);
  let parser = Parser::new_ext(&errors, options);
  let mut inside = false;
  let mut ac = String::new();
  for ev in parser {
    match ev {
      Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
        if lang.as_ref() == SML {
          inside = true;
        }
      }
      Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
        if lang.as_ref() == SML {
          if ac.starts_with("(* ok *)") {
            check_with_std_basis(ac.as_ref());
          } else if ac.starts_with("(* error *)") {
            fail_with_std_basis(ac.as_ref());
          }
          ac.clear();
          inside = false;
        }
      }
      Event::Text(s) => {
        if inside {
          ac.push_str(s.as_ref());
        }
      }
      _ => {}
    }
  }
}
