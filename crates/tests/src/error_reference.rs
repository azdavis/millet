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
  let mut buf = String::new();
  for ev in parser {
    match ev {
      Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(s))) => {
        if s.as_ref() == SML {
          inside = true;
        }
      }
      Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(s))) => {
        if s.as_ref() == SML {
          if s.starts_with("(* ok *)") {
            check(s.as_ref());
          } else if s.starts_with("(* error *)") {
            fail(s.as_ref());
          }
          buf.clear();
          inside = false;
        }
      }
      Event::Text(s) => {
        if inside {
          buf.push_str(s.as_ref());
        }
      }
      _ => {}
    }
  }
}
