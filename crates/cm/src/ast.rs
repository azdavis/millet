use smol_str::SmolStr;
use std::path::{Path, PathBuf};
use std::str::FromStr;

pub struct Pathname(PathBuf);

pub struct Name(SmolStr);

pub enum Root {
  Alias(Pathname),
  Desc(DescKind, Vec<ExportFilter>, Vec<Member>),
}

pub enum DescKind {
  Group,
  Library,
}

pub struct ExportFilter {
  pub namespace: Namespace,
  pub name: Name,
}

pub enum Namespace {
  Structure,
  Signature,
  Functor,
  FunSig,
}

pub struct Member {
  pub pathname: Pathname,
  pub class: Option<Class>,
}

impl Member {
  fn class(&self) -> Option<Class> {
    self
      .class
      .or_else(|| Class::from_path(self.pathname.0.as_path()))
  }
}

#[derive(Debug, Clone, Copy)]
pub enum Class {
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
