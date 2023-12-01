//! A thin wrapper around [`lang_srv`].

fn main() -> anyhow::Result<()> {
  panic_hook::install();
  env_logger::try_init_from_env(env_logger::Env::default().default_filter_or("error"))?;
  log::info!("start up millet lsp server");
  lang_srv::run_stdio()?;
  log::info!("shut down millet lsp server");
  Ok(())
}
