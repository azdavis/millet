use lsp_types::Url;
use serde::Deserialize;
use std::io::{self, Read};

#[derive(Deserialize)]
struct WorkspaceFile {
  files: Vec<String>,
}

#[derive(Default, Debug)]
pub struct ProjectWorkspace {
  files: Vec<Url>,
}

pub const WORKSPACE_FILE: &'static str = "millet.toml";

impl ProjectWorkspace {
  pub fn new(root: &Url) -> io::Result<Self> {
    Ok(if root.scheme() == "file" {
      // Adding a / is a bit of an ugly hack here so the join for the files specified puts them in
      // the root directory. Without it the join would replace the root directory name with the
      // filename(s).
      let root =
        Url::parse(&(root.clone().into_string() + "/")).map_err(|_| io::ErrorKind::InvalidData)?;
      match root.to_file_path() {
        Ok(mut path) => {
          path.push(WORKSPACE_FILE);
          let mut config_file = std::fs::File::open(path)?;
          let mut buf = String::new();
          config_file.read_to_string(&mut buf)?;
          let config: WorkspaceFile = toml::from_str(&buf)?;
          ProjectWorkspace {
            files: config
              .files
              .into_iter()
              .map(|f| root.join(&f))
              .collect::<Result<_, _>>()
              .map_err(|_| io::ErrorKind::InvalidData)?,
          }
        }
        Err(()) => Default::default(),
      }
    } else {
      Default::default()
    })
  }

  pub fn get_files(&self) -> &[Url] {
    &self.files
  }
}
