//! Tests to make sure Millet behaves as expected on the public documentation.

use crate::check::raw;
use diagnostic::Severity;
use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag};

const SML: &str = "sml";

fn check_all(contents: &str) {
  let mut options = Options::empty();
  options.insert(Options::ENABLE_TABLES);
  let parser = Parser::new_ext(contents, options);
  let mut inside = false;
  let mut ignore_next = false;
  let mut ac = String::new();
  let opts = raw::Opts {
    std_basis: raw::StdBasis::Full,
    outcome: raw::Outcome::Pass,
    limit: raw::Limit::First,
    min_severity: Severity::Warning,
    expected_input: raw::ExpectedInput::Good,
  };
  for ev in parser {
    match ev {
      Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
        if lang.as_ref() == SML {
          inside = true;
        }
      }
      Event::End(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
        if lang.as_ref() == SML {
          if !ignore_next {
            raw::get(raw::one_file_fs(ac.as_ref()), opts);
          }
          ac.clear();
          inside = false;
          ignore_next = false;
        }
      }
      Event::Text(s) => {
        if inside {
          ac.push_str(s.as_ref());
        }
      }
      Event::Html(s) => {
        if s.trim_start().starts_with("<!-- @ignore ") {
          ignore_next = true;
        }
      }
      _ => {}
    }
  }
}

#[test]
fn diagnostics() {
  check_all(include_str!("../../../docs/diagnostics.md"));
}

#[test]
fn tokens() {
  check_all(include_str!("../../../docs/tokens.md"));
}

#[test]
fn primitives() {
  check_all(include_str!("../../../docs/primitives.md"));
}
