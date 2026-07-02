//! Checking SML code blocks in Markdown files.

use crate::check::raw;
use diagnostic::Severity;
use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag, TagEnd};

const LANG_NAME: &str = "sml";

fn opts(limit: raw::Limit) -> raw::Opts<'static> {
  raw::Opts {
    std_basis: raw::StdBasis::Full,
    outcome: raw::Outcome::Pass,
    limit,
    min_severity: Severity::Warning,
    expected_input: raw::ExpectedInput::Good,
  }
}

const EMPTY_CONFIG: &str = "version = 1\n";

pub(crate) fn check(contents: &str) {
  let mut options = Options::empty();
  options.insert(Options::ENABLE_TABLES);
  let parser = Parser::new_ext(contents, options);
  let mut inside = false;
  let mut ignore_next = false;
  let mut limit = raw::Limit::None;
  let mut ac = String::new();
  let mut config_str = EMPTY_CONFIG.to_owned();
  for ev in parser {
    match ev {
      Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
        if lang.as_ref() == LANG_NAME {
          inside = true;
        }
      }
      Event::End(TagEnd::CodeBlock) => {
        if inside {
          if !ignore_next {
            raw::get(raw::singleton(&config_str, ac.as_ref()), opts(limit));
          }
          config_str.clear();
          config_str.push_str(EMPTY_CONFIG);
          ac.clear();
          inside = false;
          ignore_next = false;
          limit = raw::Limit::None;
        }
      }
      Event::Text(s) => {
        if inside {
          ac.push_str(s.as_ref());
        }
      }
      Event::Html(s) => {
        let s = s.trim();
        if special_comment(s, "ignore").is_some() {
          ignore_next = true;
        } else if let Some(what) = special_comment(s, "limit") {
          assert_eq!(what, "first", "only know how to limit to first");
          limit = raw::Limit::First;
        } else if let Some(x) = special_comment(s, "config") {
          config_str.push_str(x);
          config_str.push('\n');
        } else if s.starts_with("<!--") {
          panic!("unknown comment: {s}");
        }
      }
      _ => {}
    }
  }
}

fn special_comment<'c>(comm: &'c str, prefix: &str) -> Option<&'c str> {
  // is this better than allocating a big prefix to strip? :shrug:
  comm.strip_prefix("<!-- @")?.strip_prefix(prefix)?.strip_prefix(' ')?.strip_suffix(" -->")
}
