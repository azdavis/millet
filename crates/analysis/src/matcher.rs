//! Displaying a matcher, for the `fill case` code action

use fmt_util::sep_seq;
use std::fmt;

pub(crate) fn display(
  starting_bar: bool,
  variants: &[(str_util::Name, bool)],
) -> impl fmt::Display + '_ {
  CaseDisplay { starting_bar, variants }
}

struct CaseDisplay<'a> {
  starting_bar: bool,
  variants: &'a [(str_util::Name, bool)],
}

impl fmt::Display for CaseDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "  ")?;
    if self.starting_bar {
      write!(f, "| ")?;
    } else {
      write!(f, "  ")?;
    }
    let iter = self.variants.iter().map(|&(ref name, has_arg)| ArmDisplay { name, has_arg });
    sep_seq(f, "\n  | ", iter)
  }
}

struct ArmDisplay<'a> {
  name: &'a str_util::Name,
  has_arg: bool,
}

impl fmt::Display for ArmDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name)?;
    if self.has_arg {
      write!(f, " _")?;
    }
    write!(f, " => _")
  }
}
