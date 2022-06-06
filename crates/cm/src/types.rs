//! TODO prefer just a (new type) string over PathBuf?

use smol_str::SmolStr;
use std::path::{Path, PathBuf};
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq)]
pub struct Name(SmolStr);

impl Name {
  pub(crate) fn new(s: &str) -> Self {
    Self(s.into())
  }

  pub fn as_str(&self) -> &str {
    self.0.as_str()
  }
}

pub(crate) enum Root {
  Alias(PathBuf),
  Desc(DescKind, Vec<Export>, Vec<Member>),
}

pub(crate) enum DescKind {
  Group,
  Library,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Export {
  pub namespace: Namespace,
  pub name: Name,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Namespace {
  Structure,
  Signature,
  Functor,
  FunSig,
}

pub(crate) struct Member {
  pub(crate) pathname: PathBuf,
  pub(crate) class: Option<Class>,
}

impl Member {
  pub(crate) fn class(&self) -> Option<Class> {
    self
      .class
      .or_else(|| Class::from_path(self.pathname.as_path()))
  }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum Class {
  Sml,
  CMFile,
  SCGroup,
  SCLibrary,
  MLLex,
  MLYacc,
  MLBurg,
  Rcs,
  Noweb,
}

impl Class {
  fn from_path(path: &Path) -> Option<Self> {
    let ret = match path.extension()?.to_str()? {
      "sig" | "sml" | "fun" => Self::Sml,
      "grm" | "y" => Self::MLYacc,
      "lex" | "l" => Self::MLLex,
      "burg" => Self::MLBurg,
      "cm" => Self::CMFile,
      "sc" => Self::SCGroup,
      "nw" => Self::Noweb,
      _ => {
        if path.as_os_str().to_str()?.ends_with(",v") {
          Self::Rcs
        } else {
          return None;
        }
      }
    };
    Some(ret)
  }
}

impl FromStr for Class {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    let ret = match s.to_ascii_lowercase().as_str() {
      "sml" => Self::Sml,
      "cmfile" => Self::CMFile,
      "scgroup" => Self::SCGroup,
      "sclibrary" => Self::SCLibrary,
      "mllex" => Self::MLLex,
      "mlyacc" => Self::MLYacc,
      "mlburg" => Self::MLBurg,
      "rcs" => Self::Rcs,
      "noweb" => Self::Noweb,
      _ => return Err(()),
    };
    Ok(ret)
  }
}
