use crate::check::{go, Outcome, StdBasis};
use diagnostic_util::Severity;
use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag};

const SML: &str = "sml";
const MINI_STD_BASIS: &str = include_str!("mini-std-basis.sml");

fn check_all<F>(contents: &str, mut f: F)
where
  F: FnMut(&str),
{
  let mut options = Options::empty();
  options.insert(Options::ENABLE_TABLES);
  let parser = Parser::new_ext(contents, options);
  let mut inside = false;
  let mut ignore_next = false;
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
          if !ignore_next {
            f(ac.as_str());
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
        if s.trim() == "<!-- @ignore -->" {
          ignore_next = true;
        }
      }
      _ => {}
    }
  }
}

#[test]
fn errors() {
  check_all(include_str!("../../../docs/errors.md"), |s| {
    let (outcome, severity) = if s.starts_with("(* ok *)") {
      (Outcome::Pass, Severity::Error)
    } else if s.starts_with("(* error *)") {
      (Outcome::Fail, Severity::Error)
    } else if s.starts_with("(* warning *)") {
      (Outcome::Fail, Severity::Warning)
    } else {
      panic!("unsure how to handle a code block (not marked as ok, error, or ignore): {s}");
    };
    go(&[MINI_STD_BASIS, s], StdBasis::Minimal, outcome, severity);
  });
}

#[test]
fn tokens() {
  check_all(include_str!("../../../docs/tokens.md"), |s| {
    go(&[MINI_STD_BASIS, s], StdBasis::Minimal, Outcome::Pass, Severity::Error);
  });
}
