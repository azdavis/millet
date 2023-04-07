//! A thin wrapper around [`lang_srv`].

#![deny(clippy::pedantic, missing_debug_implementations, missing_docs, rust_2018_idioms)]

fn main() -> anyhow::Result<()> {
  env_logger::try_init_from_env(env_logger::Env::default().default_filter_or("error"))?;
  log::info!("start up millet lsp server");
  lang_srv::run_stdio()?;
  log::info!("shut down millet lsp server");
  Ok(())
}
