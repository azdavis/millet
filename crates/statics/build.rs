use quote::{format_ident, quote};
use std::fs::OpenOptions;
use std::io::Write as _;
use std::process::{Command, Stdio};

const TYPES: [&str; 11] = [
  "BOOL", "CHAR", "INT", "REAL", "STRING", "WORD", "EXN", "REF", "UNIT", "LIST", "ORDER",
];

fn main() {
  let types = TYPES.iter().enumerate().map(|(idx, name)| {
    let name = format_ident!("{name}");
    quote! { pub(crate) const #name: Self = Self(#idx); }
  });
  let tokens = quote! {
    #![doc = "See [`Sym`]."]

    #[doc = "Definition: TyName"]
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(crate) struct Sym(pub(super) usize);

    impl Sym {
      #(#types)*
    }
  };
  write_rust_file("src/types/sym.rs", tokens.to_string().as_str()).unwrap();
}

fn write_rust_file(name: &str, contents: &str) -> Result<(), Box<dyn std::error::Error>> {
  let mut prog = Command::new("rustfmt")
    .stdin(Stdio::piped())
    .stdout(Stdio::piped())
    .spawn()?;
  let mut stdout = prog.stdout.take().unwrap();
  let mut out_file = OpenOptions::new()
    .write(true)
    .create(true)
    .truncate(true)
    .open(name)?;
  prog.stdin.take().unwrap().write_all(contents.as_bytes())?;
  std::io::copy(&mut stdout, &mut out_file)?;
  assert!(prog.wait()?.success());
  Ok(())
}
