use crate::check::{check, fail};
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
  for ev in parser {
    match ev {
      Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(s))) => {
        if s.as_ref() == SML {
          inside = true;
        } else {
          panic!("non-{SML} code block: {s}");
        }
      }
      Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(s))) => {
        if s.as_ref() == SML {
          inside = false;
        } else {
          panic!("non-{SML} code block: {s}");
        }
      }
      Event::Text(s) => {
        if !inside {
          continue;
        }
        if s.starts_with("(* ok *)") {
          check(s.as_ref());
        } else if s.starts_with("(* error *)") {
          fail(s.as_ref());
        } else {
          panic!("code block didn't start with ok or error comment: {s}");
        }
      }
      _ => {}
    }
  }
}
