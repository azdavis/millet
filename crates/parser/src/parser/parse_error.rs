use lexer::TokenKind;
use text_size::TextRange;

use std::fmt;

#[derive(Debug, PartialEq)]
pub(crate) struct ParseError {
  pub(super) expected: Vec<TokenKind>,
  pub(super) found: Option<TokenKind>,
  pub(super) range: TextRange,
}

impl fmt::Display for ParseError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "error at {}..{}: expected ",
      u32::from(self.range.start()),
      u32::from(self.range.end()),
    )?;

    let num_expected = self.expected.len();
    let is_first = |idx| idx == 0;
    let is_last = |idx| idx == num_expected - 1;

    for (idx, expected_kind) in self.expected.iter().enumerate() {
      if is_first(idx) {
        write!(f, "{}", expected_kind)?;
      } else if is_last(idx) {
        write!(f, " or {}", expected_kind)?;
      } else {
        write!(f, ", {}", expected_kind)?;
      }
    }

    if let Some(found) = self.found {
      write!(f, ", but found {}", found)?;
    }

    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use expect_test::{expect, Expect};

  fn check(
    expected: Vec<TokenKind>,
    found: Option<TokenKind>,
    range: std::ops::Range<u32>,
    output: Expect,
  ) {
    let error = ParseError {
      expected,
      found,
      range: {
        let start = range.start.into();
        let end = range.end.into();
        TextRange::new(start, end)
      },
    };

    output.assert_eq(&format!("{}", error));
  }
}
