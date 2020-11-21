use lsp_types::Url;
use serde::Deserialize;
use std::io::{self, Read};

#[derive(Deserialize)]
struct ConfigDe {
  files: Vec<String>,
}

#[derive(Default, Debug)]
pub struct Config {
  files: Vec<Url>,
}

// TODO: maybe make into array of possible config locations?
pub const CONFIG_FILE: &'static str = "millet.toml";

impl Config {
  pub fn new(root: &Url) -> io::Result<Self> {
    Ok(if root.scheme() == "file" {
      let root =
        Url::parse(&(root.clone().into_string() + "/")).map_err(|_| io::ErrorKind::InvalidData)?;
      match root.to_file_path() {
        Ok(mut path) => {
          path.push(CONFIG_FILE);
          let mut config_file = std::fs::File::open(path)?;
          let mut buf = String::new();
          config_file.read_to_string(&mut buf)?;
          let config: ConfigDe = toml::from_str(&buf)?;
          Config {
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
