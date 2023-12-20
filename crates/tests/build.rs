//! Make the diagnostics tests.

use quote::{format_ident, quote};
use std::path::Path;

fn root_dir() -> &'static Path {
  Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().parent().unwrap()
}

fn main() {
  let entries = std::fs::read_dir(root_dir().join("docs").join("diagnostics")).unwrap();
  let tests = entries.map(|entry| {
    let entry = entry.unwrap();
    let path = entry.path();
    let number = path.file_stem().unwrap().to_str().unwrap();
    let name = format_ident!("d{number}");
    // NOTE: this has 5 parents (aka ..) in the path. we are depending on OUT_DIR being 5 levels
    // deep from the repo root. at time of writing the out dir is always like this:
    //
    // target/debug/build/tests-XXXXXX/out
    //
    // this might be a fragile assumption (cough cough Hyrum's Law).
    let include_path = format!("../../../../../docs/diagnostics/{number}.md");
    quote! {
      #[test]
      fn #name() {
        check(include_str!(#include_path));
      }
    }
  });
  let all = quote! {
    use crate::check::markdown::check;

    #(#tests)*
  };
  write_rs_tokens::go(all, "diagnostics.rs");
}
